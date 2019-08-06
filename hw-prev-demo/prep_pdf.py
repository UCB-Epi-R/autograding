"""
This script pads a student PDF submission to 15 pages to prep for gradescope upload
"""
import argparse
from PyPDF2 import PdfFileReader, PdfFileWriter

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("infile", help="path of input file to pad")
    parser.add_argument("outfile", help="path of output padded PDF")
    parser.add_argument("pad_to", help="desired total number of pages")
    return parser.parse_args()

def pad(input_path, output_path, pad_num):
    pdf_reader = PdfFileReader(input_path)
    pdf_writer = PdfFileWriter()

    pdf_writer.appendPagesFromReader(pdf_reader)

    for x in range(pad_num - pdf_reader.getNumPages()):
	    pdf_writer.addBlankPage()

    with open(output_path, 'wb') as fileobj:
        pdf_writer.write(fileobj)

def main():
    args = parse_args()
    pad(args.infile, args.outfile, args.pad_to)

if __name__ == "__main__":
    main()
