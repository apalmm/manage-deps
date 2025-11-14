from flask import Flask

app = Flask(__name__)

if __name__ == "__main__":
    # r('options(repos = c(CRAN = "https://cloud.r-project.org"))')
    app.run(debug=False, threaded=False, use_reloader=False)
