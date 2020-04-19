#text in ggplot

#https://cran.r-project.org/web/packages/ggfittext/vignettes/introduction-to-ggfittext.html

pacman::p_load(ggfittext, ggplot2)
animals
ggplot(animals, aes(x = type, y = flies, label = animal)) +  geom_tile(fill = "white", colour = "black") +  geom_fit_text()


ggplot(animals, aes(x = type, y = flies, label = animal)) +  geom_tile(fill = "white", colour = "black") +  geom_fit_text(reflow = TRUE)


ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +  geom_tile(fill = "white", colour = "black") +  geom_fit_text(reflow = TRUE, grow = TRUE)

ggplot(animals, aes(x = type, y = flies, label = animal)) +   geom_tile(fill = "white", colour = "black") +  geom_fit_text(place = "topleft", reflow = TRUE)

altitudes
ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) + geom_col() +   geom_bar_text()

coffees
ggplot(coffees, aes(x = coffee, y = proportion, label = ingredient,  fill = ingredient)) + geom_col(position = "stack") + geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE)


ggplot(coffees, aes(x = coffee, y = proportion, label = ingredient, fill = ingredient)) +  geom_col(position = "dodge") + geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, place = "left") + coord_flip()


ggplot(presidential, aes(ymin = start, ymax = end, x = party, label = name)) +
  geom_fit_text(grow = TRUE) +   geom_errorbar(alpha = 0.5)


ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
  geom_tile() +   geom_fit_text(reflow = TRUE, grow = TRUE, contrast = TRUE)


# padding.x and padding.y can be used to set the padding between the text and the edge of the box. By default this is 1 mm. These values must be given as grid::unit() objects.
# min.size sets the minimum font size in points, by default 4 pt. Text smaller than this will be hidden (see also outside).
# outside is FALSE by default for geom_fit_text(). If TRUE, text that is placed at “top”, “bottom”, “left” or “right” and must be shrunk smaller than min.size to fit in the box will be flipped to the outside of the box (if it fits there). This is mostly useful for drawing text inside bars in a bar plot, so it is TRUE by default for geom_bar_text().
# hjust and vjust set the horizontal and vertical justification of the text, scaled between 0 (left/bottom) and 1 (right/top). These are both 0.5 by default.
# formatter allows you to set a function that will be applied to the text before it is drawn. This is mostly useful in contexts where variables may be interpolated, such as when using gganimate.
# fullheight is automatically set depending on place, but can be overridden with this option. This is used to determine the bounding box around the text. If FALSE, the bounding box includes the x-height of the text and descenders, but not any descenders. If TRUE, it extends from the top of the ascenders to the bottom of the descenders. This is mostly useful in situations where you want to ensure the baseline of text is consistent between labels (fullheight = TRUE), or when you want to avoid descenders spilling out of the bounding box (fullheight = FALSE).