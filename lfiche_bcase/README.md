Laserfiche Briefcase Conversion
===============================

2012-06-01

This program saved a doctor's office a lot of money, when they
were trying to convert from Laserfiche version 6 to a full-blown EMR 
product. They had 500GB of TIFF documents that they needed to extract, 
and tie correctly to the associated patients.  To make this happen for 
them, I reverse-engineered the format of Laserfiche's export files, 
and wrote this utility to create files with a readable file-name and a 
searchable index.

The code was written in 2012, in Literate C++.  It was originally
written in a custom litprog tool, but I converted it later to 
vanilla `cweb`.

  * **readbriefcase.pdf**: The woven literate program.
  * **readbriefcase.w**: The raw cweb C++ source.

