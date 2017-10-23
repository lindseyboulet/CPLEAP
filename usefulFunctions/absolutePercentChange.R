library(plyr)

df <- ddply(df,.(indepVar1, indepVar2), # Column names of independent variables
            transform,newColumnName= # Type a new column name here, no quotes
              ((depVarColumnName-depVarColumnName[2])/ #This code tell the computer  to subtract row 2 from                                                          # row 1 (square brackets) – this might only work if \                                                          # you have 2 conditions, ie baseline and hypoxia. 
                                                        # You may have to play with the numbers inside the                                                            #  square brackets to get it right. 
                 depVarColumnName[2])*100)  # Remove this section to get absolute change
