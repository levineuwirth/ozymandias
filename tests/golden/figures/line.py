"""The fixture's static figure (.figure): an SVG on stdout, drawn in black
for the build to recolour, from the standard library alone."""

print('<svg xmlns="http://www.w3.org/2000/svg" width="120pt" height="60pt" viewBox="0 0 120 60">'
      '<polyline points="10,50 40,20 70,35 110,10" fill="none" stroke="#000000"/></svg>')
