FROM python:3.8-slim

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends chromium && \
    pip install --upgrade pip setuptools wheel pipenv


WORKDIR /app

COPY Pipfile .
COPY Pipfile.lock .
RUN pipenv install --dev --system && \
    pip install chromedriver-binary~=$(chromium --version | perl -pe 's/([^0-9]+)([0-9]+\.[0-9]+).+/$2/g')
COPY main.py .
COPY contents contents

VOLUME /app/inputs /app/solutions
ARG input=input.txt
ARG solution=solution.txt

CMD ["python", "./main.py", "./inputs/$input", "./solutions/$solution"]
