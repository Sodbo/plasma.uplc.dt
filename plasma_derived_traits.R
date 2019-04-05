plasma_derived_traits = function(my_data){
	my_data = my_data[,grepl(colnames(my_data),pattern='GP')]
	
	GP = rowSums(my_data)
	GPn = rowSums(my_data[,paste0('GP',c(1:11,18))])
	my_data$PGP37 = with(my_data,(GP13+GP15+GP20+GP26+GP28) / (GP4+GP5+GP10+GP13+GP15+GP20+GP26+GP28) * 100)
	my_data$PGP38 = with(my_data,(GP16+GP21) / (GP6+GP11+GP16+GP21) * 100)
	
	my_data$PGP39 = with(my_data,(GP13+GP15+GP20+GP26+GP28) / (GP1+GP4+GP5+GP10+GP13+GP15+GP20+GP26+GP28) * 100)
	
	my_data$PGP40 = with(my_data,(GP16+GP21) / (GP2+GP6+GP11+GP16+GP21) * 100)
	
	my_data$PGP41 = with(my_data,GP13 / (GP4+GP5+GP13) * 100)
	
	my_data$PGP42 = with(my_data,GP15 / (GP10+GP15+GP20) * 100)
	my_data$PGP43= with(my_data,GP20 /  (GP10+GP15+GP20) * 100)
	
	my_data$PGP44 = with(my_data,GP16 /  (GP11+GP16+GP21) * 100)
	
	my_data$PGP45 = with(my_data,GP21 /  (GP11+GP16+GP21) * 100)
	
	my_data$PGP46 = with(my_data,(GP13+GP15+GP16) / (GP20+GP21))
	
	my_data$PGP47= with(my_data,(GP13+GP15) / GP20)
	
	my_data$PGP48 = with(my_data,GP16 / GP21)
	
	my_data$PGP49 = with(my_data,(GP13+GP15+GP16) / (GP26+GP28))
	
	my_data$PGP50 = with(my_data,(GP13+GP15) / (GP26+GP28))
	
	my_data$PGP51 = with(my_data,(GP20+GP21) / (GP26+GP28))
	
	my_data$PGP52 = with(my_data,(GP20) / (GP26+GP28))
	
	my_data$PGP53 = with(my_data,(GP16+GP21) / (GP13+GP15+GP20+GP26+GP28))
	
	my_data$PGP54 = with(my_data,GP16 / (GP13+GP15))
	
	my_data$PGP55 = with(my_data,GP16 / (GP13+GP15+GP16))
	
	my_data$PGP56 = with(my_data,GP21 / GP20)
	
	my_data$PGP57 = with(my_data,GP21 / (GP20+GP21))
	
	my_data$PGP58 = with(my_data,GP1 / GPn * 100)
	
	my_data$PGP59 = with(my_data,GP2 / GPn * 100)
	
	my_data$PGP60= with(my_data,GP3 / GPn * 100)
	
	my_data$PGP61 = with(my_data,GP4 / GPn * 100)
	
	my_data$PGP62 = with(my_data,GP5 / GPn * 100)
	
	my_data$PGP63 = with(my_data,GP6 / GPn * 100)
	
	my_data$PGP64 = with(my_data,GP7 / GPn * 100)
	
	my_data$PGP65 = with(my_data,GP8 / GPn * 100)
	
	my_data$PGP66 = with(my_data,GP9 / GPn * 100)
	
	my_data$PGP67 = with(my_data,GP10 / GPn * 100)
	
	my_data$PGP68 = with(my_data,GP11 / GPn * 100)
	my_data$PGP69 = with(my_data,GP18 / GPn * 100)
	
	my_data$PGP70 = with(my_data,PGP58+PGP59)
	
	my_data$PGP71 = with(my_data,PGP60+PGP61+PGP62+PGP63)
	
	my_data$PGP72 = with(my_data,PGP65+PGP66+PGP67+PGP68)
	
	my_data$PGP73 = with(my_data,PGP58+PGP59+PGP61+PGP62+PGP63+PGP67+PGP68)
	
	my_data$PGP74 = with(my_data,(PGP61+PGP62+PGP63) / PGP71 * 100)
	
	my_data$PGP75 = with(my_data,(PGP67+PGP68) / PGP72  * 100)
	
	my_data$PGP76 = with(my_data,PGP58+PGP61+PGP62+PGP67)
	
	my_data$PGP77 = with(my_data,PGP58 / PGP70 * 100)
	
	my_data$PGP78 = with(my_data,(PGP61+PGP62) / PGP71 * 100)
	
	my_data$PGP79 = with(my_data,PGP67 / PGP72 * 100)
	
	my_data$PGP80 = with(my_data,PGP59+PGP63+PGP68)
	
	my_data$PGP81 = with(my_data,PGP59 / PGP70 * 100)
	
	my_data$PGP82 = with(my_data,PGP63 / PGP71 * 100)
	
	my_data$PGP83 = with(my_data,PGP68 / PGP72 * 100)
	
	my_data$PGP84 = with(my_data,PGP80 / PGP76)
	
	my_data$PGP85 = with(my_data,PGP80 / PGP73 * 100)
	
	my_data$PGP86 = with(my_data,PGP76 / (PGP80+PGP60+PGP66))
	
	my_data$PGP87 = with(my_data,(PGP60+PGP66) / (PGP76+PGP80) * 100)
	
	my_data$PGP88 = with(my_data,PGP68 / PGP67)
	
	my_data$PGP89 = with(my_data,PGP68 / (PGP67+PGP68) * 100)
	
	my_data$PGP90 = with(my_data,PGP67 / (PGP66+PGP68))
	
	my_data$PGP91  = with(my_data,PGP66 / (PGP67+PGP68))
	my_data$PGP92 = with(my_data,GP29+GP32+GP36)
	
	my_data$PGP93 = with(my_data,GP1+GP2+GP4+GP5+GP6+GP10+GP11+GP13+GP15+GP16+GP20+GP21+GP26+GP28)
	
	my_data$PGP94 = with(my_data,GP1+GP2+GP3+GP4+GP5+GP6+GP7+GP8+GP9+GP10+GP11)
	
	my_data$PGP95 = with(my_data,GP12+GP13+GP14+GP15+GP16)
	
	my_data$PGP96 = with(my_data,GP17+GP19+GP20+GP21+GP22+GP23)
	
	my_data$PGP97 = with(my_data,GP24+GP25+GP26+GP27+GP28+GP29+GP30+GP31+GP32)
	
	my_data$PGP98 = with(my_data,GP33+GP34+GP35+GP36)
	
	my_data$PGP99 = with(my_data,GP1+GP2)
	
	my_data$PGP100 = with(my_data,GP3+GP4+GP5+GP6+GP12+GP13)
	
	my_data$PGP101 = with(my_data,GP8+GP9+GP10+GP11+GP14+GP15+GP16+GP17+GP19+GP20+GP21)
	
	my_data$PGP102 = with(my_data,GP22+GP23+GP24+GP25+GP26+GP27+GP28+GP29+GP32)
	
	my_data$PGP103 = with(my_data,GP30+GP31+GP33+GP34+GP35+GP36)
	
	my_data$PGP104 = with(my_data,GP1+GP2+GP3+GP4+GP5+GP6+GP8+GP9+GP10+GP11+GP12+GP13+GP14+GP15+GP16+GP17+GP19+GP20+GP21)
	
	my_data$PGP105 = with(my_data,GP22+GP23+GP24+GP25+GP26+GP27+GP28+GP29)
	
	my_data$PGP106 = with(my_data,GP30+GP31+GP32+GP33+GP34+GP35+GP36)
	my_data$PGP107 = with(my_data,GP2+GP7+GP18)
	my_data$PGP108 = with(my_data,GP2+GP3+GP6+GP9+GP11+GP12+GP16+GP21)
	my_data$PGP109 = with(my_data,(GP22+GP23) / (GP24+GP25+GP26+GP27+GP28+GP29+GP32))
	my_data$PGP110 = with(my_data,(GP30+GP31) / (GP33+GP34+GP35+GP36))
	my_data$PGP111 = with(my_data,(GP26+GP28) / (GP22+GP23+GP24+GP25+GP26+GP27+GP28+GP29+GP32) * 100)
	my_data$PGP112 = with(my_data,(GP29+GP32) / (GP22+GP23+GP24+GP25+GP26+GP27+GP28+GP29+GP32) * 100)
	my_data$PGP113 = with(my_data,GP36 / (GP30+GP31+GP33+GP34+GP35+GP36) * 100)
	my_data = my_data[,-c(1:36)]
	return(my_data)
}
