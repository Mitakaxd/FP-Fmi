import Data.Char;
normalize str= 
     if any isDigit str 
	     then  error "no digits allowed" 
		 else map toUpper [x | x<-str, isLetter x]
encode alphabet ch offset
    |elem ch alphabet==False = error ("elem not supported: "++[ch])	 
		|offset<0 =  encode alphabet ch (offset+(length alphabet))
			|otherwise =(!!offset) (dropWhile (\alch -> if alch==ch then False else True) (cycle alphabet))	
			
encrypt alphabet offset normalized=map (\ch -> encode alphabet ch offset) normalized
decrypt alphabet offset encrypted = encrypt alphabet (0-offset) encrypted

crackall alphabet encrypted = map (\key -> decrypt alphabet key encrypted) [1..num]
	where num = ((length alphabet)-1)
substring sub str
	|null str = False
		|take (length sub) str== sub = True 
			|otherwise = substring sub (tail str)	
			
crackcandidates alphabet commonwords encrypted=[word | word<-(crackall alphabet encrypted),any (\comword -> substring comword word) commonwords]
polyencrypt alphabet offset step blockSize normalized= 
     if null normalized 
	     then [] 
		 else (encrypt alphabet offset (take blockSize normalized))++(polyencrypt alphabet (offset+step) step blockSize (drop blockSize normalized))
polydecrypt alphabet offset step blockSize encrypted = 
     if null encrypted 
	     then [] 
		 else (decrypt alphabet offset (take blockSize encrypted))++(polydecrypt alphabet (offset+step) step blockSize (drop blockSize encrypted))

enigmaencrypt alphabet rotors normalized= 
     if null rotors 
	     then normalized 
		 else (enigmaencrypt alphabet (tail rotors)(polyencrypt alphabet (fst3 (head rotors)) (second3 (head rotors))(third3 (head rotors)) normalized))
	where  
	fst3(a,_,_)=a
	second3(_,a,_)=a
	third3(_,_,a)=a
enigmadecrypt alphabet rotors normalized = 
     if null rotors 
	     then normalized 
		 else (enigmadecrypt alphabet (init rotors)(polydecrypt alphabet (fst3 (last rotors)) (second3 (last rotors))(third3 (last rotors)) normalized))
	         where 
			 fst3(a,_,_)=a
			 second3(_,a,_)=a
			 third3(_,_,a)=a
