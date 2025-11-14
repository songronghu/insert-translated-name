import requests
import sys

# 从命令行参数读取待翻译的文本和源目标语言
input_text = sys.argv[1]
source_language = "auto"  # 可根据需要修改源语言
target_language = "en"    # 目标语言为英文

url = 'http://localhost:5000/translate'

data = {
        "q": input_text,
        "source": source_language,
        "target": target_language,
        "format": "text"
        }

response = requests.post(url, json=data)
if response.status_code == 200:
    result = response.json()['translatedText']
    print(result, flush=True)
else:
    print(f"Error: {response.status_code}", flush=True)
