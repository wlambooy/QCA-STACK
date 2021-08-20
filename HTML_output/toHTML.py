import sys
from ansi2html import Ansi2HTMLConverter

if len(sys.argv) != 2:
	print ("Usage: python toHTML.py <infile>")

fname = sys.argv[1]

inf = open (sys.argv[1], "r")
outf = open (fname.replace('.txt','') + '.html', "w")

html = Ansi2HTMLConverter().convert(inf.read())
html = html.replace('&amp;','&').replace('&lt;','<').replace('&gt;','>')
html = html.replace('<pre class="ansi2html-content">','<pre>')

outf.write(html)

print("done.")

inf.close()
outf.close()
