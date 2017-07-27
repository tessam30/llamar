#' Colors: internal colors based on Vega and Tableau colors
#' From https://github.com/vega/vega/wiki/Scales
#' 
#' 

category10 = c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf")

category20 = c("#1f77b4","#aec7e8","#ff7f0e","#ffbb78","#2ca02c","#98df8a","#d62728","#ff9896","#9467bd","#c5b0d5","#8c564b","#c49c94","#e377c2","#f7b6d2","#7f7f7f","#c7c7c7","#bcbd22","#dbdb8d","#17becf","#9edae5")

category20b = c("#393b79","#5254a3","#6b6ecf","#9c9ede","#637939","#8ca252","#b5cf6b","#cedb9c","#8c6d31","#bd9e39","#e7ba52","#e7cb94","#843c39","#ad494a","#d6616b","#e7969c","#7b4173","#a55194","#ce6dbd","#de9ed6")

category20c = c("#3182bd","#6baed6","#9ecae1","#c6dbef","#e6550d","#fd8d3c","#fdae6b","#fdd0a2","#31a354","#74c476","#a1d99b","#c7e9c0","#756bb1","#9e9ac8","#bcbddc","#dadaeb","#636363","#969696","#bdbdbd","#d9d9d9")

tableau10 = c("#4e79a7", "#F28E2C","#E15759","#76B7B2","#59A14F","#EDC949","#AF7AA1","#FF9DA7","#9C755F","#BAB0AB")

tableau20 = c("#4e79a7", "#a0cbe8","f28e2b","ffbe7d", "59A14F","8cd17d","b6992d","f1ce63", "499894","86bcb6","E15759","ff9d9a", "79706e","BAB0AB","d37295","fabfd2", "b07aa1","d4a6c8","9d7660","d7b5a6")

save(category10, file = "data/category10.rda")
save(category20, file = "data/category20.rda")
save(category20b, file = "data/category20b.rda")
save(category20c, file = "data/category20c.rda")
save(tableau10, file = "data/tableau10.rda")
save(tableau20, file = "data/tableau20.rda")
