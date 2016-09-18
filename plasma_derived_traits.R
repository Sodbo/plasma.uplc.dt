plasma_derived_traits = function(my_data){

	my_data = my_data[,grepl(colnames(my_data),pattern='GP')]

	GP = rowSums(my_data)
	GPn = rowSums(my_data[,1:11])

	my_data$GGP37 = with(my_data,(GP13+GP16+GP22+GP28+GP30+GP31+GP34) / (GP1+GP4+GP5+GP10+GP13+GP16+GP22+GP28+GP30+GP31+GP34) * 100)

	my_data$GGP38 = with(my_data,(GP17+GP23) / (GP2+GP6+GP11+GP17+GP23) * 100)
	
	my_data$GGP39 = with(my_data,(GP13+GP16+GP22+GP28+GP30+GP31+GP34+GP38) / (GP1+GP4+GP5+GP10+GP13+GP16+GP22+GP28+GP30+GP31+GP34+GP38) * 100)
	
	my_data$GGP40 = with(my_data,(GP17+GP23) / (GP2+GP6+GP11+GP17+GP23) * 100)
	
	my_data$GGP41 = with(my_data,GP13 / (GP4+GP5+GP13) * 100)
	
	my_data$GGP42 = with(my_data,GP16 / (GP10+GP16+GP22) * 100)

	my_data$GGP43= with(my_data,GP22 /  (GP10+GP16+GP22) * 100)
	
	my_data$GGP44 = with(my_data,GP17 /  (GP11+GP17+GP23) * 100)
	
	my_data$GGP45 = with(my_data,GP23 /  (GP11+GP17+GP23) * 100)
	
	my_data$GGP46 = with(my_data,(GP13+GP16+GP17) / (GP22+GP23))
	
	my_data$GGP47= with(my_data,(GP13+GP16) / GP22)
	
	my_data$GGP48 = with(my_data,GP17 / GP23)
	
	my_data$GGP49 = with(my_data,(GP13+GP16+GP17) / (GP28+GP30+GP31
		+GP34))
	
	my_data$GGP50 = with(my_data,(GP13+GP16) / (GP28+GP30+GP31+GP34))
	
	my_data$GGP51 = with(my_data,(GP13+GP16+GP17) / (GP38))
	
	my_data$GGP52 = with(my_data,(GP13+GP16) / (GP38))
	
	my_data$GGP53 = with(my_data,(GP22+GP23) / (GP28+GP30+GP31+GP34))
	
	my_data$GGP54 = with(my_data,(GP22) / (GP28+GP30+GP31+GP34))
	
	my_data$GGP55 = with(my_data,(GP22+GP23) / (GP38))
	
	my_data$GGP56 = with(my_data,(GP22) / (GP38))
	
	my_data$GGP57 = with(my_data,(GP28+GP30+GP31+GP34) / (GP38))
	
	my_data$GGP58 = with(my_data,(GP17+GP23) / (GP13+GP16+GP22+GP28+GP30+GP31+GP34+GP38))
	
	my_data$GGP59 = with(my_data,GP17 / (GP13+GP16))
	
	my_data$GGP60 = with(my_data,GP17 / (GP13+GP16+GP17))
	
	my_data$GGP61 = with(my_data,GP23 / GP22)
	
	my_data$GGP62 = with(my_data,GP23 / (GP22+GP23))
	
	my_data$GGP63 = with(my_data,GP1 / GPn * 100)
	
	my_data$GGP64 = with(my_data,GP2 / GPn * 100)
	
	my_data$GGP65= with(my_data,GP3 / GPn * 100)
	
	my_data$GGP66 = with(my_data,GP4 / GPn * 100)
	
	my_data$GGP67 = with(my_data,GP5 / GPn * 100)
	
	my_data$GGP68 = with(my_data,GP6 / GPn * 100)
	
	my_data$GGP69 = with(my_data,GP7 / GPn * 100)
	
	my_data$GGP70 = with(my_data,GP8 / GPn * 100)
	
	my_data$GGP71 = with(my_data,GP9 / GPn * 100)
	
	my_data$GGP72 = with(my_data,GP10 / GPn * 100)
	
	my_data$GGP73 = with(my_data,GP11 / GPn * 100)
	
	my_data$GGP74 = with(my_data,GGP63+GGP64+GGP69)
	
	my_data$GGP75 = with(my_data,GGP65+GGP66+GGP67+GGP68)
	
	my_data$GGP76 = with(my_data,GGP70+GGP71+GGP72+GGP73)
	
	my_data$GGP77 = with(my_data,GGP63+GGP64+GGP66+GGP67+GGP68+GGP72+GGP73)
	
	my_data$GGP78 = with(my_data,(GGP63+GGP64) / GGP74 * 100)
	
	my_data$GGP79 = with(my_data,(GGP66+GGP67+GGP68) / GGP75 * 100)
	
	my_data$GGP80 = with(my_data,(GGP72+GGP73) / GGP76  * 100)
	
	my_data$GGP81 = with(my_data,GGP63+GGP66+GGP67+GGP72)
	
	my_data$GGP82 = with(my_data,GGP63 / GGP74 * 100)
	
	my_data$GGP83 = with(my_data,GGP66+GGP67 / GGP75 * 100)
	
	my_data$GGP84 = with(my_data,GGP72 / GGP76 * 100)
	
	my_data$GGP85 = with(my_data,GGP64+GGP68+GGP73)
	
	my_data$GGP86 = with(my_data,GGP64 / GGP74 * 100)
	
	my_data$GGP87 = with(my_data,GGP68 / GGP75 * 100)
	
	my_data$GGP88 = with(my_data,GGP73 / GGP76 * 100)
	
	my_data$GGP89 = with(my_data,GGP85 / GGP81)
	
	my_data$GGP90 = with(my_data,GGP85 / GGP81 * 100)
	
	my_data$GGP91 = with(my_data,GGP81 / (GGP85+GGP65+GGP71) * 100)
	
	my_data$GGP92 = with(my_data,(GGP65+GGP71) / (GGP81+GGP85) * 1000)
	
	my_data$GGP93 = with(my_data,GGP73 / GGP72)
	
	my_data$GGP94 = with(my_data,GGP73 / (GGP72+GGP73) * 100)
	
	my_data$GGP95 = with(my_data,GGP72 / (GGP71+GGP73))
	
	my_data$GGP96  = with(my_data,GGP71 / (GGP72+GGP73))

	my_data$GGP97 = with(my_data,GP31+GP34+GP38)
	
	my_data$GGP98 = with(my_data,GP1+GP2+GP4+GP5+GP6+GP10+GP11+GP13+GP16+GP17+GP22+GP23+GP28+GP30)
	
	my_data$GGP99 = with(my_data,GP1+GP2+GP3+GP4+GP5+GP6+GP7+GP8+GP9+GP10+GP11)
	
	my_data$GGP100 = with(my_data,GP12+GP13+GP14.15+GP16+GP17)
	
	my_data$GGP101 = with(my_data,GP18+GP19+GP20.21+GP22+GP23+GP24+GP25)
	
	my_data$GGP102 = with(my_data,GP26+GP27+GP28+GP29+GP30+GP31+GP32+GP33+GP34)
	
	my_data$GGP103 = with(my_data,GP35+GP36+GP37+GP38)
	
	my_data$GGP104 = with(my_data,GP1+GP2+GP7)
	
	my_data$GGP105 = with(my_data,GP3+GP4+GP5+GP6+GP12+GP13)
	
	my_data$GGP106 = with(my_data,GP8+GP9+GP10+GP11+GP14.15+GP16+GP17+GP18+GP19+GP20.21+GP22+GP23)
	
	my_data$GGP107 = with(my_data,GP24+GP25+GP26+GP27+GP28+GP29+GP30+GP31+GP34)
	
	my_data$GGP108 = with(my_data,GP32+GP33+GP35+GP36+GP37+GP38)
	
	my_data$GGP109 = with(my_data,GP1+GP2+GP3+GP4+GP5+GP6+GP8+GP9+GP10+GP11+GP12+GP13+GP14.15+GP16+GP17+GP18+GP20.21+GP22+GP23)
	
	my_data$GGP110 = with(my_data,GP19+GP24+GP25+GP26+GP27+GP28+GP29+GP30+GP31)
	
	my_data$GGP111 = with(my_data,GP33+GP33+GP34+GP35+GP36+GP37+GP38)

	return(my_data)

}