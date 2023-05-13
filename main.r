
ball <- function(n){
    #中獎連線種類(用矩陣方式表現)
#      [1] [2] [3]
#  [1]  1   2   3
#  [2]  4   5   6
#  [3]  7   8   9
	win_line <- c(123, 456, 789, 147, 258, 369, 159, 357)

	SuccessCount <- 0    #中獎次數統計
	UnSuccessCount <- 0  #未中獎次數統計
    
    tmpVec <- c(0,0,0)
    tmpNum = 0

	for (i in 1:n){
        # choice answer
        ans = sample(win_line, 1)
        tmpNum = ans
        for (j in 3:1){
            tmpVec[j] <- tmpNum%%10
            tmpNum <- (tmpNum - tmpVec[j])/10
        }
        print("answer")
        print(tmpVec)
        print("指定中獎連線")
        show <- matrix(c('x', 'x', 'x','x','x','x','x','x','x'), nrow = 3, ncol = 3)
        for (j in 1:3){
            num = tmpVec[j]
            if ((num%%3) == 0){
                y <- num/3
                x <- 3
            }else{
                y <- num/3+1
                x <- num%%3
            }
            show[y, x] = 'o'
        }
        print(show)

        # throw ball
        temp <- sample(1:9, 3)
        tmpVec <- c(temp[1], temp[2], temp[3])
        check <- 0
            # sorting
        while (check != 2){
                check <- 0
            for (j  in 1:2){
                if (tmpVec[j] < tmpVec[j+1]){
                    check <- check + 1
                    next
                    }
                        tmp <- tmpVec[j]
                        tmpVec[j] <- tmpVec[j+1]
                        tmpVec[j+1] <- tmp

            }
            
        }
        ballPoint <- tmpVec[1]*100 + tmpVec[2]*10 + tmpVec[3]	
        
        show <- matrix(c('x', 'x', 'x','x','x','x','x','x','x'), nrow = 3, ncol = 3)
        for (j in 1:3){
            num = tmpVec[j]
            if ((num%%3) == 0){
                y <- num/3
                x <- 3
            }else{
                y <- num/3+1
                x <- num%%3
            }
            show[y, x] = 'o'
        }
        print("落下球的位置")
        print(show)


        if (ballPoint == ans){
            SuccessCount <- SuccessCount+1
            print("恭喜中獎")
        }else{
            UnSuccessCount <- UnSuccessCount + 1
            print("失敗, 再一次")
            
        }
        print("剩餘次數")
        print(n-i)
        print("==========================")

	}
    print("中獎次數")
    print(SuccessCount)
    print("失敗次數")
    print(UnSuccessCount)
    output <- c("中獎" =SuccessCount, "未中獎" = UnSuccessCount)

    print(output)

    barplot(output, main = paste("執行次數",n))
}

ball(1000) #<-- 在此輸入執行(丟擲)次數