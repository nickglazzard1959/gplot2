txt = """\\begin{{figure}}
  \\centering
  \\includegraphics[width=\\maxwidth]{{{0}}}
  \\caption{{{1}}}
  \\label{{fig:{2}}}
\\end{{figure}}
"""

with open('../../obey-files/fetch-list', 'r') as fin:
    flist = fin.readlines()

n = 0
for fname in flist:
    fname = fname.strip()
    outstring = txt.format('../../temp/'+fname+'.eps', fname, fname)
    print(outstring)
    n += 1
    if (n % 3) == 0:
        print('\\clearpage')
    
