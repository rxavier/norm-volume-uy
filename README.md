# Normative volume

This R script scrapes the [IMPO](https://www.impo.com.uy/cgi-bin/bases/consultaBasesBS.cgi?tipoServicio=3) website for laws and decrees approved since 1985.

The script also prunes the law/decree count from "immaterial" norms. This is a WIP: currently I have selected keywords associated with unimportant norms and remove any norm that matches that keyword. Surely some of the removed ones are actually important.

Data is then seasonally decomposed to obtain the SA and trend components which are then plotted.

This type of data is not new. For example, [Facultad de Ciencias Sociales](http://cienciassociales.edu.uy/bancosdedatos/gobierno/) has data for laws from 1985 onwards that also considers who wrote and sent the bill to Parliament and whether it was approved or not. However, there's no data for Executive decrees and they are presented in annual frequency.
