#!/bin/bash

R -e 'devtools::build_manual(path = "doc")'
# libreoffice --invisible --infilter="writer_pdf_import" --convert-to docx:"MS Word 2007 XML" rcontroll_0.1.0.pdf
R -e 'rmarkdown::render(input = "vignettes/troll.Rmd", output_file = "../doc/troll.docx", output_format = "word_document")'
R -e 'rmarkdown::render(input = "vignettes/workflow.Rmd", output_file = "../doc/workflow.docx", output_format = "word_document")'
R -e 'rmarkdown::render(input = "vignettes/calibration.Rmd", output_file = "../doc/calibration.docx", output_format = "word_document")'
R -e 'rmarkdown::render(input = "vignettes/Schmittetal2020.Rmd", output_file = "../doc/Schmittetal2020.docx", output_format = "word_document")'
