from flask import Flask, render_template, send_from_directory, request, jsonify
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

app = Flask(__name__)

# Serve main page
@app.route("/")
def index():
    return render_template("index.html")

#serve generated graph files or data
@app.route("/data/<path:filename>")
def data_files(filename):
    return send_from_directory("data", filename)

#serve generated graph HTML files
@app.route("/graphs/<path:filename>")
def serve_graph(filename):
    return send_from_directory("data/graphs", filename)

#API endpoint for dependency analysis
@app.route("/analyze", methods=["POST"])
def analyze():
    data = request.get_json()
    func = data.get("function")

    from rpy2.robjects import conversion, default_converter
    with conversion.localconverter(default_converter):
        codetools = importr("codetools")
        tools = importr("tools")

        deps = set()
        try:
            pkg = robjects.r(f'find("{func}")')[0].replace("package:", "")
            deps.add(pkg)
            globals_ = codetools.findGlobals(robjects.r(func))
            for sym in globals_:
                try:
                    owner = robjects.r(f'find("{sym}")')[0].replace("package:", "")
                    deps.add(owner)
                except Exception:
                    pass
        except Exception as e:
            return jsonify({"error": str(e)})

    full = tools.package_dependencies(list(deps), recursive=True)
    return jsonify({"required_packages": list(deps), "tree": str(full)})

if __name__ == "__main__":
    app.run(debug=True)