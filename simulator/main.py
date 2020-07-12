import argparse
import os

from selenium.webdriver import Chrome, ChromeOptions
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.ui import WebDriverWait

# to resolve chromedriver binary path
import chromedriver_binary

# example: `python main.py <description_file> <solution_file>`

parser = argparse.ArgumentParser('run official simulator in selenium')
parser.add_argument('input_file')
parser.add_argument('solution_file')
args = parser.parse_args()

desc = os.path.abspath(args.input_file)
sol = os.path.abspath(args.solution_file)

script_path = os.path.dirname(os.path.abspath(__file__))

url = 'file://' + script_path + '/contents/index.html'

base_path = os.getcwd()

if not os.path.isabs(sol):
    sol = os.path.normpath(os.path.join(base_path, sol))
else:
    sol = os.path.normpath(sol)

options = ChromeOptions()
# ヘッドレスモードを有効にする（次の行をコメントアウトすると画面が表示される）。
options.add_argument('--headless')
options.add_argument('--no-sandbox')
# ChromeのWebDriverオブジェクトを作成する。
driver = Chrome(options=options)


try:
    driver.get(url)

    assert 'Checker' in driver.title

    # find input model file
    main_element = WebDriverWait(driver, 1000).until(
        expected_conditions.presence_of_element_located((By.CLASS_NAME, "main"))
    )


    text = main_element.text

    print(text)
# スクリーンショットを撮る。
#driver.save_screenshot('tracer.png')

finally:
    driver.quit()  # ブラウザーを終了する

