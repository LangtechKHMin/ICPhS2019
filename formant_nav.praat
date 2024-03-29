clearinfo
Create Sound from formula: "StartBell", 1, 0, 1, 44100, "1/2 * exp(-6.25*x)*sin(2*pi*440*x)"
Play
select all
Remove



Create Strings as file list: "wavlist", ".\nav\*.wav"
Replace all: ".wav", "", 0, "literals"
#확장자 제거
nwav = Get number of strings


if not fileReadable: ".\formant_nav.csv"
	writeFileLine: ".\formant_nav.csv", "speakercode", "," ,"sentencecode", "," , "gender", ",", "label", ",","F1",",","F2",",", "Lang",",","level",",","tStart",",","tEnd",",","dur"
endif
clearinfo


tierNum = 1



for f to nwav
	selectObject: "Strings wavlist_replaced"
	filename$ = Get string: 'f' 	
	wavname$ = "Sound " + filename$
	gridname$ = "TextGrid " + filename$
		
	Read from file: ".\nav\" + filename$ + ".wav"
	Read from file: ".\nav\" + filename$ + ".TextGrid"

	selectObject: gridname$
	intervalNum = Get number of intervals: 'tierNum'
	
	speakercode$ = left$(filename$, 6)
	gendercode$ = mid$(speakercode$,5,2)
	sentencecode$ = mid$(filename$,8,14)
	
	if gendercode$ = "01" or gendercode$ = "02" or gendercode$ = "04"  or gendercode$ = "06"  or gendercode$ = "08"  or gendercode$ = "09" or gendercode$ = "10" or gendercode$ = "11"or gendercode$ = "15"or gendercode$ = "16"or gendercode$ = "21"or gendercode$ = "22"
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

				appendFileLine: ".\formant_nav.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L1",",","null",",",'timeStart',",",'timeEnd',",",'dur'
			endif
		endfor
	endif
	
	
	if gendercode$ = "07" or gendercode$ = "13" or gendercode$ = "14" or gendercode$ = "23" or gendercode$ = "24"
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

				appendFileLine: ".\formant_nav.csv", speakercode$, "," ,sentencecode$, "," , gender$ , ",", label$, ",",'f1',",",'f2',",","L1",",","null",",",'timeStart',",",'timeEnd',",",'dur'
			endif
		endfor
	endif
			
endfor



Create Sound from formula: "StartBell", 1, 0, 1, 44100, "1/2 * exp(-6.25*x)*sin(2*pi*660*x)"
Play
select all
Remove
exitScript: "Good Job!!! See the CSV file!"
