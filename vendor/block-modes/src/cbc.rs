use block_cipher_trait::generic_array::GenericArray;
use block_cipher_trait::generic_array::typenum::Unsigned;
use block_cipher_trait::BlockCipher;
use block_padding::Padding;
use traits::BlockMode;
use utils::{xor, get_par_blocks, ParBlocks, Block};
use core::marker::PhantomData;

/// [Cipher Block Chaining][1] (CBC) block cipher mode isntance.
///
/// [1]: https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#CBC
pub struct Cbc<C: BlockCipher, P: Padding> {
    cipher: C,
    iv: GenericArray<u8, C::BlockSize>,
    _p: PhantomData<P>,
}

impl<C: BlockCipher, P: Padding> Cbc<C, P> {
    #[inline(always)]
    fn single_blocks_decrypt(&mut self, blocks: &mut [Block<C>]) {
        let mut iv = self.iv.clone();
        for block in blocks {
            let block_copy = block.clone();
            self.cipher.decrypt_block(block);
            xor(block, iv.as_slice());
            iv = block_copy;
        }
        self.iv = iv;
    }
}

impl<C: BlockCipher, P: Padding> BlockMode<C, P> for Cbc<C, P> {
    fn new(cipher: C, iv: &Block<C>) -> Self {
        Self {
            cipher,
            iv: iv.clone(),
            _p: Default::default(),
        }
    }

    fn encrypt_blocks(&mut self, blocks: &mut [Block<C>]) {
        self.iv = {
            let mut iv = &self.iv;
            for block in blocks {
                xor(block, &iv);
                self.cipher.encrypt_block(block);
                iv = block;
            }
            iv.clone()
        };
    }

    fn decrypt_blocks(&mut self, blocks: &mut [Block<C>]) {
        let pbn = C::ParBlocks::to_usize();
        if pbn != 1 {
            let (par_blocks, leftover) = get_par_blocks::<C>(blocks);
            let mut iv_buf = ParBlocks::<C>::default();
            iv_buf[0] = self.iv.clone();
            for pb in par_blocks {
                iv_buf[1..].clone_from_slice(&pb[..pbn-1]);
                let next_iv = pb[pbn - 1].clone();
                self.cipher.decrypt_blocks(pb);
                pb.iter_mut().zip(iv_buf.iter()).for_each(|(a, b)| xor(a, b));
                iv_buf[0] = next_iv;
            }
            self.iv = iv_buf[0].clone();
            self.single_blocks_decrypt(leftover);
        } else {
            self.single_blocks_decrypt(blocks);
        }
    }
}
