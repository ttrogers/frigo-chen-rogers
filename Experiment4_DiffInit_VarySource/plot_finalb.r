function (tmp1) 
{
	#Establish plot frame
    plot(120, 1, type = "n", xlim = c(120, 180), 
		ylim = c(1, 4), yaxt = "n", ylab = "", xlab = "Shape")
		
	#Confidence interval for initial boundary in left group
		rect(tmp1[11, 3] - (tmp1[11, 6]) * qt(0.975, tmp1[11, 4]), 
        0, tmp1[11, 3] + (tmp1[11, 6]) * qt(0.975, tmp1[11, 4]), 
        6, col = hsv(1, 0.2, 1), border = NA)

	#Confidence interval for initial boundary in right group
		rect(tmp1[12, 3] - (tmp1[12, 6]) * qt(0.975, tmp1[12, 4]), 
        0, tmp1[12, 3] + (tmp1[12, 6]) * qt(0.975, tmp1[12, 4]), 
        6, col = hsv(1/3, 0.2, 1), border = NA)
		
	#Plot boundary
	box()
	
	#Add t2 boundary for each group and condition
    points(tmp1$boundary[3:10], c(4, 4, 3, 3, 2, 2, 1, 1), pch = 16, 
        cex = 2, col = c(2, 3, 2, 3, 2, 3, 2, 3, 2, 3))
 
	#Y axis labels
	mtext(side = 2, at = c(4, 3, 2, 1), text = c("Close", "Dead", 
        "Mid", "Far"), adj = 1, srt = 2)
		
    abline(v = 150, lty = 2) #midpoint dashed line
    abline(v = tmp1$boundary[11], col = 2, lwd = 2) #
    abline(v = tmp1$boundary[12], col = 3, lwd = 2)
    lines(tmp1$boundary[c(3, 5, 7, 9)], c(4, 3, 2, 1), lwd = 2, 
        col = 2)
    lines(tmp1$boundary[c(4, 6, 8, 10)], c(4, 3, 2, 1), lwd = 2, 
        col = 3)
    arrows(tmp1$boundary[3:10], c(4, 4, 3, 3, 2, 2, 1, 1), tmp1$boundary[3:10] + 
        tmp1$SErr[3:10], c(4, 4, 3, 3, 2, 2, 1, 1), col = c(2, 
        3, 2, 3, 2, 3, 2, 3), angle = 90, length = 0.1)
    arrows(tmp1$boundary[3:10], c(4, 4, 3, 3, 2, 2, 1, 1), tmp1$boundary[3:10] - 
        tmp1$SErr[3:10], c(4, 4, 3, 3, 2, 2, 1, 1), col = c(2, 
        3, 2, 3, 2, 3, 2, 3), angle = 90, length = 0.1)
}
