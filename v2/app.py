from flask import Flask

app = Flask(__name__)

if __name__ == "__main__":
    app.run(debug=False, threaded=False, use_reloader=False)
