clearinfo
Create Sound from formula: "StartBell", 1, 0, 1, 44100, "1/2 * exp(-6.25*x)*sin(2*pi*440*x)"
Play
select all
Remove



Create Strings as file list: "wavlist", ".\efl\*.wav"
Replace all: ".wav", "", 0, "literals"
#확장자 제거
nwav = Get number of strings



if not fileReadable: ".\formant_efl.csv"
	writeFileLine: ".\formant_efl.csv", "speakercode", "," ,"sentencecode", "," , "gender", ",", "label", ",","F1",",","F2",",", "Lang",",","level",",","tStart",",","tEnd",",","dur"
endif
clearinfo


tierNum = 1



for f to nwav
	selectObject: "Strings wavlist_replaced"
	filename$ = Get string: 'f' 	
	wavname$ = "Sound " + filename$
	gridname$ = "TextGrid " + filename$
		
	Read from file: ".\efl\" + filename$ + ".wav"
	Read from file: ".\efl\" + filename$ + ".TextGrid"

	selectObject: gridname$
	intervalNum = Get number of intervals: 'tierNum'
	
	speakercode$ = left$(filename$, 6)
	gendercode$ = mid$(speakercode$,5,2)
	sentencecode$ = mid$(filename$,8,14)
	
	if speakercode$ = "khs005" or speakercode$ = "khs006" or speakercode$ = "hpg007"
	# male / female difference : 5000 / 5500
		gender$ = "m"
	
		selectObject: wavname$
		To Formant (burg): 0, 5, 5000, 0.025, 50.0

		for i to intervalNum
			selectObject: gridname$
			label$ = Get label of interval: 'tierNum', 'i'
			if label$ = "sil" or label$ = "iy" or label$ = "ih" or label$ = "ey" or label$ = "eh" or label$ = "ae" or label$ = "aa" or label$ = "ao" or label$ = "uh" or label$ = "ow" or label$ = "uw" or label$ = "ax" or label$ = "aw" or label$ = "ay" or label$ = "ey" or label$ = "ow" or label$ = "oy"
		
				appendInfo: "..." + newline$
				timeStart = Get start time of interval: 'tierNum', 'i'
				timeEnd = Get end time of interval: 'tierNum', 'i' 
				dur = timeEnd - timeStart				
			
				selectObject: "Formant " + filename$
				f1 = Get quantile: 1, 'timeStart', 'timeEnd', "hertz", 0.30
				f2 = Get quantile: 2, 'timeStart', 'timeEnd', "hertz", 0.30

				if f1 == undefined
					f1 = 0.0
				endif
				if f2 == undefined
					f2 = 0.0
				endif
				
				appendFileLine: ".\formant_efl.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L2",",","IM",",",'timeStart',",",'timeEnd',",",'dur'

			endif
		endfor
	endif
	
	if speakercode$ = "khs019" or speakercode$ = "khs029" or speakercode$ = "kpg001"
		gender$ = "f"
		
		selectObject: wavname$
		To Formant (burg): 0, 5, 5500, 0.025, 50.0

		for i to intervalNum
			selectObject: gridname$
			label$ = Get label of interval: 'tierNum', 'i'
			if label$ = "sil" or label$ = "iy" or label$ = "ih" or label$ = "ey" or label$ = "eh" or label$ = "ae" or label$ = "aa" or label$ = "ao" or label$ = "uh" or label$ = "ow" or label$ = "uw" or label$ = "ax" or label$ = "aw" or label$ = "ay" or label$ = "ey" or label$ = "ow" or label$ = "oy"
		
				appendInfo: "..." + newline$
				timeStart = Get start time of interval: 'tierNum', 'i'
				timeEnd = Get end time of interval: 'tierNum', 'i' 
				dur = timeEnd - timeStart				
			
				selectObject: "Formant " + filename$
				f1 = Get quantile: 1, 'timeStart', 'timeEnd', "hertz", 0.30
				f2 = Get quantile: 2, 'timeStart', 'timeEnd', "hertz", 0.30

				if f1 == undefined
					f1 = 0.0
				endif
				if f2 == undefined
					f2 = 0.0
				endif
				
				appendFileLine: ".\formant_efl.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L2",",","IM",",",'timeStart',",",'timeEnd',",",'dur'

			endif
		endfor
	endif
	
	if speakercode$ = "hpg001" or speakercode$ = "khs011" or speakercode$ = "kug004"
		gender$ = "m"
		
		selectObject: wavname$
		To Formant (burg): 0, 5, 5000, 0.025, 50.0

		for i to intervalNum
			selectObject: gridname$
			label$ = Get label of interval: 'tierNum', 'i'
			if label$ = "sil" or label$ = "iy" or label$ = "ih" or label$ = "ey" or label$ = "eh" or label$ = "ae" or label$ = "aa" or label$ = "ao" or label$ = "uh" or label$ = "ow" or label$ = "uw" or label$ = "ax" or label$ = "aw" or label$ = "ay" or label$ = "ey" or label$ = "ow" or label$ = "oy"
		
				appendInfo: "..." + newline$
				timeStart = Get start time of interval: 'tierNum', 'i'
				timeEnd = Get end time of interval: 'tierNum', 'i' 
				dur = timeEnd - timeStart				
			
				selectObject: "Formant " + filename$
				f1 = Get quantile: 1, 'timeStart', 'timeEnd', "hertz", 0.30
				f2 = Get quantile: 2, 'timeStart', 'timeEnd', "hertz", 0.30

				if f1 == undefined
					f1 = 0.0
				endif
				if f2 == undefined
					f2 = 0.0
				endif
				
				appendFileLine: ".\formant_efl.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L2",",","IH",",",'timeStart',",",'timeEnd',",",'dur'

			endif
		endfor
	endif
	
	if speakercode$ = "hpg003" or speakercode$ = "hpg010" or speakercode$ = "khs016"
		gender$ = "f"
	
		selectObject: wavname$
		To Formant (burg): 0, 5, 5500, 0.025, 50.0

		for i to intervalNum
			selectObject: gridname$
			label$ = Get label of interval: 'tierNum', 'i'
			if label$ = "sil" or label$ = "iy" or label$ = "ih" or label$ = "ey" or label$ = "eh" or label$ = "ae" or label$ = "aa" or label$ = "ao" or label$ = "uh" or label$ = "ow" or label$ = "uw" or label$ = "ax" or label$ = "aw" or label$ = "ay" or label$ = "ey" or label$ = "ow" or label$ = "oy"
		
				appendInfo: "..." + newline$
				timeStart = Get start time of interval: 'tierNum', 'i'
				timeEnd = Get end time of interval: 'tierNum', 'i' 
				dur = timeEnd - timeStart				
			
				selectObject: "Formant " + filename$
				f1 = Get quantile: 1, 'timeStart', 'timeEnd', "hertz", 0.30
				f2 = Get quantile: 2, 'timeStart', 'timeEnd', "hertz", 0.30

				if f1 == undefined
					f1 = 0.0
				endif
				if f2 == undefined
					f2 = 0.0
				endif
				
				appendFileLine: ".\formant_efl.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L2",",","IH",",",'timeStart',",",'timeEnd',",",'dur'

			endif
		endfor
	endif
	
	if speakercode$ = "hpg005"
		gender$ = "m"
	
		selectObject: wavname$
		To Formant (burg): 0, 5, 5000, 0.025, 50.0

		for i to intervalNum
			selectObject: gridname$
			label$ = Get label of interval: 'tierNum', 'i'
			if label$ = "sil" or label$ = "iy" or label$ = "ih" or label$ = "ey" or label$ = "eh" or label$ = "ae" or label$ = "aa" or label$ = "ao" or label$ = "uh" or label$ = "ow" or label$ = "uw" or label$ = "ax" or label$ = "aw" or label$ = "ay" or label$ = "ey" or label$ = "ow" or label$ = "oy"
		
				appendInfo: "..." + newline$
				timeStart = Get start time of interval: 'tierNum', 'i'
				timeEnd = Get end time of interval: 'tierNum', 'i' 
				dur = timeEnd - timeStart				
			
				selectObject: "Formant " + filename$
				f1 = Get quantile: 1, 'timeStart', 'timeEnd', "hertz", 0.30
				f2 = Get quantile: 2, 'timeStart', 'timeEnd', "hertz", 0.30

				if f1 == undefined
					f1 = 0.0
				endif
				if f2 == undefined
					f2 = 0.0
				endif
				
				appendFileLine: ".\formant_efl.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L2",",","AD",",",'timeStart',",",'timeEnd',",",'dur'

			endif
		endfor
	endif
	
	if speakercode$ = "hpg009" or speakercode$ = "khs022" or speakercode$ = "khs025" or speakercode$ = "kpg002" or speakercode$ = "kpg003"
		gender$ = "f"
		
		selectObject: wavname$
		To Formant (burg): 0, 5, 5500, 0.025, 50.0

		for i to intervalNum
			selectObject: gridname$
			label$ = Get label of interval: 'tierNum', 'i'
			if label$ = "sil" or label$ = "iy" or label$ = "ih" or label$ = "ey" or label$ = "eh" or label$ = "ae" or label$ = "aa" or label$ = "ao" or label$ = "uh" or label$ = "ow" or label$ = "uw" or label$ = "ax" or label$ = "aw" or label$ = "ay" or label$ = "ey" or label$ = "ow" or label$ = "oy"
		
				appendInfo: "..." + newline$
				timeStart = Get start time of interval: 'tierNum', 'i'
				timeEnd = Get end time of interval: 'tierNum', 'i' 
				dur = timeEnd - timeStart				
			
				selectObject: "Formant " + filename$
				f1 = Get quantile: 1, 'timeStart', 'timeEnd', "hertz", 0.30
				f2 = Get quantile: 2, 'timeStart', 'timeEnd', "hertz", 0.30

				if f1 == undefined
					f1 = 0.0
				endif
				if f2 == undefined
					f2 = 0.0
				endif
				
				appendFileLine: ".\formant_efl.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L2",",","AD",",",'timeStart',",",'timeEnd',",",'dur'

			endif
		endfor
	endif
	
	
endfor




Create Sound from formula: "StartBell", 1, 0, 1, 44100, "1/2 * exp(-6.25*x)*sin(2*pi*660*x)"
Play
select all
Remove
exitScript: "Good Job!!! See the CSV file!"


