"""
This script automatically uploads a PDF to Gradescope.
NOTE: The --replace flag may currently be broken on Gradescope's end--
need to investigate.
There are two ways to upload to gradescope:
	1. default - This uploads a new submission. Use this if no problems have
				 been graded already for this student on this assignment,
				 otherwise reuploading will wipe out grades that have already
				 been added on gradescope.
	   To run: 'python upload_submission.py' on the autograder
	2. replace - This replaces a student's pdf on gradescope if one had already
				 been submitted for this assignment. Use this if some grades
				 had already been added on gradescope.
	   To run: 'python upload_submission.py --replace'
"""
import argparse
import requests
import pandas as pd
import os, sys

# The info package is only defined on the autograder server.
try:
	from info import info
except ImportError:
	info = {'email': 'nolanpokpongkiat@gmail.com'}

COURSE = "51507"
ASSIGNMENT = "223391" ### FILL ME IN
TOKEN = "zah6y_fc4_l_uaW_s3yU1A"
PDF_NAME = "hw_iteration.pdf"
GRADESCOPE_URL = "https://www.gradescope.com/api/v1/courses/" + \
	COURSE + "/assignments/" + ASSIGNMENT + "/submissions"

# Read in OK <-> Gradescope email map. Useful when students have
# different emails on OK and Gradescope.
ok_gs = {}

# original
# OK_GS_CSV, ok_gs_df = "ok_gs.csv", None
# if not os.path.isfile(OK_GS_CSV):
# 	sys.exit("ERROR: ok_gs.csv not found.")
# ok_gs_df = pd.read_csv(OK_GS_CSV)
# ok_gs = pd.Series(ok_gs_df["Gradescope Email"].values,
# 	index=ok_gs_df["OK Email"]).to_dict()

def upload_submission(course_id, assignment_id, student_email, filename, replace):
	url = GRADESCOPE_URL.format(course_id, assignment_id)
	s = requests.Session()
	form_data = {"owner_email": student_email}
	files = {'pdf_attachment': open(filename, 'rb')}
	request_headers = {'access-token': TOKEN}

	replace_url = url
	if replace:
		replace_url = url + '/replace_pdf'

	num_attempts = 0
	while num_attempts < 2: # Control how many times to retry
		if num_attempts > 0 and student_email in ok_gs:
			print("Retrying with OK/GS mapping")
			form_data["owner_email"] = ok_gs[student_email]

		r = s.post(replace_url, data=form_data, headers=request_headers, files=files)
		if r.status_code == 200:
			sys.exit("Upload success!")
		if r.status_code != 200:
			num_attempts += 1

	# Report error
	print('Issue uploading to Gradescope. Gradescope error output below:')
	error_lines = json.loads(r._content)["pdf_file"]
	for line in error_lines:
		print(line)
	print("Upload URL:", url)

if __name__ == '__main__':
	parser = argparse.ArgumentParser()
	parser.add_argument('--replace', action='store_true')
	args = parser.parse_args()
	replace = vars(args)['replace']
	email = info['email']
	upload_submission(COURSE, ASSIGNMENT, email, PDF_NAME, replace)
