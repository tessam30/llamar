#' Colors: internal colors based on RColorBrewer, Viridis, Vega and Tableau colors
#' From https://github.com/vega/vega/wiki/Scales
#' 
#' @import ggplot2
#' 
#' @export
#' @examples 
#' # Sequential, warm tones
#' plot_color_palette('Reds')
#' plot_color_palette('OrRd')
#' plot_color_palette('Oranges')
#' plot_color_palette('YlOrRd')
#' plot_color_palette('YlOrBr')
#' plot_color_palette('RdPu')
#' plot_color_palette('PuRd')
#' plot_color_palette('magma')
#' plot_color_palette('inferno')
#' plot_color_palette('magma')
#' plot_color_palette('plasma')
#' # Sequential, cool tones
#' plot_color_palette('Greens')
#' plot_color_palette('YlGn')
#' plot_color_palette('YlGnBu')
#' plot_color_palette('viridis')
#' plot_color_palette('BuGn')
#' plot_color_palette('GnBu')
#' plot_color_palette('PuBuGn')
#' plot_color_palette('Blues')
#' plot_color_palette('Purples')
#' plot_color_palette('BuPu')
#' plot_color_palette('Greys')
#' 
#' # Diverging
#' plot_color_palette('RdYlGn')
#' plot_color_palette('RdYlBu')
#' plot_color_palette('RdBu')
#' plot_color_palette('RdGy')
#' plot_color_palette('Spectral')
#' plot_color_palette('PiYG')
#' plot_color_palette('PRGn')
#' plot_color_palette('PuOr')
#' plot_color_palette('BrBG')

#' # Categorical
#' plot_color_palette('category10')
#' plot_color_palette('category20')
#' plot_color_palette('category20b')
#' plot_color_palette('category20c')
#' plot_color_palette('tableau10')
#' plot_color_palette('tableau20')
#' plot_color_palette('Paired')
#' plot_color_palette('Pastel1')
#' plot_color_palette('Pastel2')
#' plot_color_palette('Accent')
#' plot_color_palette('Dark2')
#' plot_color_palette('Set1')
#' plot_color_palette('Set2')
#' plot_color_palette('Set3')

plot_color_palette = function(color_name) {
  
  get_colors = function (color_name) {
    color = tryCatch(eval(as.name(color_name)), error = function(e) NULL)
    return(color)
  }
  
  colors = get_colors(color_name)
  
  if(is.null(colors)) {
    stop("Unknown color palette supplied")
  } else{
    num_colors = length(colors)
  }
  
  ggplot(data.frame(x = letters[1:num_colors]), aes(x = x, y = 1, fill = x)) + 
    geom_tile() + 
    scale_fill_manual(values = colors, guide = FALSE) + 
    coord_fixed(ratio  = 0.3) +
    theme_void() + 
    ggtitle(color_name)
}

define_colors = function(save_colors = FALSE){
  # Color Brewer sequential palettes
  Reds = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D")
  Oranges = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704")
  OrRd    = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000")
  YlOrRd  = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
  YlOrBr  = c("#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#993404", "#662506")
  YlGn    = c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529")
  Greens  = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B")
  BuGn    = c("#F7FCFD", "#E5F5F9", "#CCECE6", "#99D8C9", "#66C2A4", "#41AE76", "#238B45", "#006D2C", "#00441B")
  YlGnBu  = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")
  GnBu    = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081")
  PuBuGn  = c("#FFF7FB", "#ECE2F0", "#D0D1E6", "#A6BDDB", "#67A9CF", "#3690C0", "#02818A", "#016C59", "#014636")
  Blues   = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
  PuBu    = c("#FFF7FB", "#ECE7F2", "#D0D1E6", "#A6BDDB", "#74A9CF", "#3690C0", "#0570B0", "#045A8D", "#023858")
  Purples = c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
  BuPu    = c("#F7FCFD", "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B")
  RdPu    = c("#FFF7F3", "#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177", "#49006A")
  PuRd    = c("#F7F4F9", "#E7E1EF", "#D4B9DA", "#C994C7", "#DF65B0", "#E7298A", "#CE1256", "#980043", "#67001F")
  Greys   = c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000")
  
  # Color Brewer diverging palettes 
  PiYG    = c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA", "#FDE0EF", "#F7F7F7", "#E6F5D0", "#B8E186", "#7FBC41", "#4D9221", "#276419")
  PRGn    = c("#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#F7F7F7", "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B")
  PuOr    = c("#7F3B08", "#B35806", "#E08214", "#FDB863", "#FEE0B6", "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC", "#542788", "#2D004B")
  BrBG    = c("#543005", "#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", "#F5F5F5", "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#003C30")
  RdYlGn  = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")
  RdYlBu  = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")
  RdBu    = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
  RdGy    = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#E0E0E0", "#BABABA", "#878787", "#4D4D4D", "#1A1A1A")
  Spectral = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
  
  # Color Brewer categorical palettes 
  Accent  = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666")
  Dark2   = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
  Paired  = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
  Pastel1 = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2")
  Pastel2 = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")
  Set1    = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
  Set2    = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
  Set3    = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
  
  # Viridis sequential palettes
  viridis = rev(c("#440154FF", "#481B6DFF", "#46337EFF", "#3F4889FF", "#365C8DFF", "#2E6E8EFF", "#277F8EFF", "#21908CFF", "#1FA187FF", "#2DB27DFF", "#4AC16DFF", "#71CF57FF", "#9FDA3AFF", "#CFE11CFF", "#FDE725FF"))
  inferno = rev(c("#000004FF", "#0D082AFF", "#280B54FF", "#480B6AFF", "#65156EFF", "#82206CFF", "#9F2A63FF", "#BB3754FF", "#D44842FF", "#E8602DFF", "#F57D15FF", "#FB9E07FF", "#FAC127FF", "#F3E55CFF", "#FCFFA4FF"))
  magma   = rev(c("#000004FF", "#0C0927FF", "#231151FF", "#410F75FF", "#5F187FFF", "#7B2382FF", "#982D80FF", "#B63679FF", "#D3436EFF", "#EB5760FF", "#F8765CFF", "#FD9969FF", "#FEBA80FF", "#FDDC9EFF", "#FCFDBFFF"))
  plasma  = rev(c("#0D0887FF", "#350498FF", "#5402A3FF", "#7000A8FF", "#8B0AA5FF", "#A31E9AFF", "#B93289FF", "#CC4678FF", "#DB5C68FF", "#E97158FF", "#F48849FF", "#FBA139FF", "#FEBC2AFF", "#FADA24FF", "#F0F921FF"))
  
  
  # Vega palettes
  category10 = c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf")
  
  category20 = c("#1f77b4","#aec7e8","#ff7f0e","#ffbb78","#2ca02c","#98df8a","#d62728","#ff9896","#9467bd","#c5b0d5","#8c564b","#c49c94","#e377c2","#f7b6d2","#7f7f7f","#c7c7c7","#bcbd22","#dbdb8d","#17becf","#9edae5")
  
  category20b = c("#393b79","#5254a3","#6b6ecf","#9c9ede","#637939","#8ca252","#b5cf6b","#cedb9c","#8c6d31","#bd9e39","#e7ba52","#e7cb94","#843c39","#ad494a","#d6616b","#e7969c","#7b4173","#a55194","#ce6dbd","#de9ed6")
  
  category20c = c("#3182bd","#6baed6","#9ecae1","#c6dbef","#e6550d","#fd8d3c","#fdae6b","#fdd0a2","#31a354","#74c476","#a1d99b","#c7e9c0","#756bb1","#9e9ac8","#bcbddc","#dadaeb","#636363","#969696","#bdbdbd","#d9d9d9")
  
  # Tableau categorical palettes
  tableau10 = c("#4e79a7", "#F28E2C","#E15759","#76B7B2","#59A14F","#EDC949","#AF7AA1","#FF9DA7","#9C755F","#BAB0AB")
  
  tableau20 = c("#4e79a7", "#a0cbe8","#f28e2b","#ffbe7d", "#59A14F","#8cd17d","#b6992d","#f1ce63", "#499894","#86bcb6","#E15759","#ff9d9a", "#79706e","#BAB0AB","#d37295","#fabfd2", "#b07aa1","#d4a6c8","#9d7660","#d7b5a6")
  
  # USAID brand colors
  USAID_blue = '#002F6C'
  USAID_red = '#BA0C2F'
  USAID_medblue = '#0067B9'
  USAID_ltblue = '#A7C6ED'
  USAID_dkred = '#651D32'
  
  ftfBlue = "#4799B5"
  ftfGreen = "#94A545"
  ftfOrange = "#D37D28"
  
  
  if(save_colors == TRUE){
    
    colors = c('Reds', 'Oranges', 
               'OrRd',   
               'YlOrRd', 
               'YlOrBr', 
               'YlGn',   
               'Greens', 
               'BuGn',   
               'YlGnBu', 
               'GnBu',   
               'PuBuGn', 
               'Blues',  
               'PuBu',   
               'Purples',
               'BuPu',   
               'RdPu',   
               'PuRd',   
               'Greys',  
               'PiYG',   
               'PRGn',   
               'PuOr',   
               'BrBG',   
               'RdYlGn', 
               'RdYlBu', 
               'RdBu',   
               'RdGy',   
               'Spectral',
               'Accent', 
               'Dark2',  
               'Paired',
               'Pastel1',
               'Pastel2',
               'Set1',   
               'Set2',   
               'Set3', 
               'inferno',
               'magma',
               'plasma',
               'viridis',
               'category10',
               'category20',
               'category20b',
               'category20c', 
               'tableau10', 'tableau20',
               'USAID_blue',
               'USAID_red',
               'USAID_medblue',
               'USAID_ltblue',
               'USAID_dkred',
               'ftfBlue',
               'ftfGreen',
               'ftfOrange')
    
    lapply(colors, function(x) save(list = x, file = paste0('data/', x, '.rda')))
  }
}