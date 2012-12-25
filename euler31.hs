main = do
	 let res = length [[twoL,oneL,fiftyP,twentyP,tenP,fiveP,twoP,oneP] |
                  twoL <- [0,200..200],
                  oneL <- [0,100..200-twoL],
               fiftyP  <- [0,50..200-twoL-oneL],
               twentyP <- [0,20..200-twoL-oneL-fiftyP],
               tenP    <- [0,10..200-twoL-oneL-fiftyP-twentyP],
               fiveP   <- [0,5..200-twoL-oneL-fiftyP-twentyP-tenP],
               twoP    <- [0,2..200-twoL-oneL-fiftyP-twentyP-tenP-fiveP],
               oneP    <- [0..200-twoL-oneL-fiftyP-twentyP-tenP-fiveP-twoP],
               twoL + oneL + fiftyP + twentyP + tenP + fiveP + twoP + oneP == 200]
	 print res
