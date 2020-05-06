# Quick start with the qqman library
install.packages("qqman")

# Load the library
library(qqman)

# Make the Manhattan plot on the gwasResults dataset- Requires 4 columns identified properly CHR, BP, SNP, P
manhattan(gwasResults, chr="CHR", bp="BP", snp="SNP", p="P")

# Highlight a group of SNP on the Manhattan plot- provide this with a list of SNPs of interest with the library
snpsOfInterest

# Highlight them on the plot
manhattan(gwasResults, highlight = snpsOfInterest)

# Annotate these SNPS on the plot
manhattan(gwasResults, annotatePval=0.01)

# Highlight SNPs Let’s suppose the you have a group of SNP that you want to highlight on the plot. This can be done following almost the same procedure. We just need to add them a flag in the dataframe, and use the flag for the color
# List of SNPs to highlight are in the snpsOfInterest object
# We will use ggrepel for the annotation
install.packages("ggrepel")
library(ggrepel)

# Prepare the dataset-- CAUTION: ERROR EXISTS HERE, R DOESNT LIKE % SYMBOLS. NEED TO EDIT.
don <- gwasResults %>% 
  
  # Compute chromosome size
  group_by(CHR) %>% 
  summarise(chr_len=max(BP)) %>% 
  
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
  
  # Add a cumulative position of each SNP
  arrange(CHR, BP) %>%
  mutate( BPcum=BP+tot) %>%
  
  # Add highlight and annotation information
  mutate( is_highlight=ifelse(SNP %in% snpsOfInterest, "yes", "no")) %>%
  mutate( is_annotate=ifelse(-log10(P)>4, "yes", "no")) 

# Prepare X axis-- PROBABLY NEED TO EDIT OUT % SYMBOLS HERE
axisdf <- don %>% group_by(CHR) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )

# Make the plot
ggplot(don, aes(x=BPcum, y=-log10(P))) +
  
  # Show all points
  geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
  scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
  
  # custom X axis:
  scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
  scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
  # Add highlighted points
  geom_point(data=subset(don, is_highlight=="yes"), color="orange", size=2) +
  
  # Add label using ggrepel to avoid overlapping
  geom_label_repel( data=subset(don, is_annotate=="yes"), aes(label=SNP), size=2) +
  
  # Custom the theme:
  theme_bw() +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )