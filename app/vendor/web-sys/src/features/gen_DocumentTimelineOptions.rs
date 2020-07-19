#![allow(unused_imports)]
use super::*;
use wasm_bindgen::prelude::*;
#[wasm_bindgen]
extern "C" {
    # [ wasm_bindgen ( extends = :: js_sys :: Object , js_name = DocumentTimelineOptions ) ]
    #[derive(Debug, Clone, PartialEq, Eq)]
    #[doc = "The `DocumentTimelineOptions` dictionary."]
    #[doc = ""]
    #[doc = "*This API requires the following crate features to be activated: `DocumentTimelineOptions`*"]
    pub type DocumentTimelineOptions;
}
impl DocumentTimelineOptions {
    #[doc = "Construct a new `DocumentTimelineOptions`."]
    #[doc = ""]
    #[doc = "*This API requires the following crate features to be activated: `DocumentTimelineOptions`*"]
    pub fn new() -> Self {
        #[allow(unused_mut)]
        let mut ret: Self = ::wasm_bindgen::JsCast::unchecked_into(::js_sys::Object::new());
        ret
    }
    #[doc = "Change the `originTime` field of this object."]
    #[doc = ""]
    #[doc = "*This API requires the following crate features to be activated: `DocumentTimelineOptions`*"]
    pub fn origin_time(&mut self, val: f64) -> &mut Self {
        use wasm_bindgen::JsValue;
        let r = ::js_sys::Reflect::set(
            self.as_ref(),
            &JsValue::from("originTime"),
            &JsValue::from(val),
        );
        debug_assert!(
            r.is_ok(),
            "setting properties should never fail on our dictionary objects"
        );
        let _ = r;
        self
    }
}
