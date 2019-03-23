# Normative volume
(Or something in proper English)

This R script scrapes Uruguay's Presidency website which contains every decree passed and every law promulgated by the Executive branch since 2000.

Strictly, it's missing laws approved by Parliament that haven't been promulgated by the Executive but there should be very few of them.

The script also prunes the law/decree count from "immaterial" norms. This is a WIP: currently I have selected keywords associated with unimportant norms and remove any norm that matches that keyword. Surely some of the removed ones are actually important. There's an auxiliary script that breaks down every norm into words and makes it easy to see which ones are associated with norms we're not interested in.

Data is then seasonally decomposed to obtain the SA and trend components which are then plotted.

This type of data is not new. For example, [Facultad de Ciencias Sociales](http://cienciassociales.edu.uy/bancosdedatos/gobierno/) has data for laws between 1985-2015 that also considers who wrote and sent the bill to Parliament and whether it was approved or not. However, there's no data for Executive decrees, the last available year is 2015 and they are presented in annual frequency.
