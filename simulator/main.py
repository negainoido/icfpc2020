import os
import sys
# to resolve chromedriver binary path
import chromedriver_binary

from selenium.webdriver import Chrome, ChromeOptions
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.ui import WebDriverWait

# example: `python main.py <description_file> <solution_file>`

script_path = os.path.dirname(os.path.abspath(__file__))

desc = sys.argv[1]
sol = sys.argv[2]

url = 'file://' + script_path + '/contents/index.html'

base_path = os.getcwd()

if not os.path.isabs(desc):
    desc = os.path.normpath(os.path.join(base_path, desc))
else:
    desc = os.path.normpath(desc)

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

