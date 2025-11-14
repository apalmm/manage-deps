from flask import Flask
# import multiprocessing as mp
# from rpy2.robjects import r

app = Flask(__name__)

# def r_worker(job_q, result_q):
#     while True:
#         func = job_q.get()
#         if func is None:
#             break
#         try:
#             deps = r(f"analyze_function_deps('{func}')")
#             result_q.put(list(deps))
#         except Exception as e:
#             result_q.put({"error": str(e)})

# job_q = mp.Queue()
# result_q = mp.Queue()

# # Start R worker process (single-threaded)
# worker = mp.Process(target=r_worker, args=(job_q, result_q))
# worker.start()

if __name__ == "__main__":
    app.run(debug=False, threaded=False, use_reloader=False)
