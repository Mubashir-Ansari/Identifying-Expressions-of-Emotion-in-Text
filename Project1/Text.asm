TITLE MY COAL PROJECT
INCLUDE Irvine32.inc
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


NoFile PROTO, fileName:PTR BYTE
storeNoEmotionWord PROTO, noEmotionWord:PTR BYTE, noEmotionWordSize:DWORD
countEmotions PROTO,NoOfEmotions:DWORD
wordFind PROTO, src: ptr byte, key: ptr byte, strSize: dword, keySize: dword
printEmotion PROTO, EmotionWord:PTR BYTE, EmotionWordSize:DWORD
printFoundWord PROTO, foundWord:PTR BYTE
clear PROTO, textString:PTR BYTE, StringLength:DWORD
extracted_Word PROTO
;---------------------------------------------------------------------------------------------------- DATA BLOCK -------------------------------------------------------------------------------------------------------------

.data

EmotionStringSize DWORD 10000
EmotionString BYTE 10000 dup(0)
EmotionCount BYTE 6 DUP(?)
EC BYTE ?
largest SBYTE -1
position DWORD ?

FileNames byte  "Happy.txt",0,0,0,0,0,0,0,0,0,0,0
		  byte  "Sad.txt",0,0,0,0,0,0,0,0,0,0,0,0,0
		  byte  "Anger.txt",0,0,0,0,0,0,0,0,0,0,0
		  byte  "Disgust.txt",0,0,0,0,0,0,0,0,0
		  byte  "Fear.txt",0,0,0,0,0,0,0,0,0,0,0,0
		  byte  "None.txt",0,0,0,0,0,0,0,0,0,0,0

EmotionNum DWORD 6
EmotionLength DWORD 400
TempFileNames DWORD  ?

EmotionfileHandler DWORD 0

inputFile BYTE "Input.txt",0
inputString BYTE 20000 dup(0)
inputFileHandler DWORD 0
currentInputIndex DWORD offset inputString  
extractedWord BYTE 400 dup(0)
extractedWordSize DWORD 0

EmotionWordsCount BYTE  20000 dup(0)
IndexNoEmotionWords DWORD offset EmotionWordsCount

lineOut	BYTE 40 dup(?)
uword byte 40 dup(?)


;//loop counters
mainLoopCounter DWORD 20000
world_len byte ?


;//flags
inputFileEnded DWORD 0
fileEmotionWritten DWORD 0
lastWord DWORD 0


;//Strings to be used
semiColon BYTE ":",0
dot BYTE "."
bigSpace BYTE "       ",0
new_line byte 0Dh,0Ah


;//prompts
promptEnter byte "Enter text for identifying expressions of emotion: ",0
promptDisplay byte "sentence entered",0 
promptFile1 BYTE "File '",0
promptFile2 BYTE "' does not exist or cannot be opened.",0
promptNoDot BYTE "The is no '.' terminator in the input. '.' determines the end of input. Exiting the program.",0
promptHappy BYTE "Your entered sentence suggests that you posses HAPPY emotion :)",0
promptSad BYTE "Your entered sentence suggests that you posses SAD emotion :(",0
promptAnger BYTE "Your entered sentence suggests that you posses ANGER emotion :()",0
promptDisgust BYTE "Your entered sentence suggests that you posses DISGUST emotion :)(",0
promptFear BYTE "Your entered sentence suggests that you posses FEAR emotion :!)",0
promptMixed BYTE "Your entered sentence suggests that you posses MIXED emotion :)",0
promptNone BYTE "Your entered sentence suggests that you posses NO emotion :0 OR it might be NEGATION of the analysis made by the program",0
welcome BYTE "                                              WELCOME TO TEXT EMOTIONS IDENTIFIER",0
dotline BYTE "---------------------------------------------------------------------------------------------------------------------------",0
disclaimer BYTE "DISCLAIMER--> This disclaimer informs users that the analysis expressed through the project belong not necessarily to any individual.",0
BlueTextOnMagenta = white + (magenta * 16)
DefaultColor = magenta + (white * 16)

;-------------------------------------------------------------------------------------------------------- CODE BLOCK ---------------------------------------------------------------------------------------------------------
.code

main PROC
;--------------------------------------------------------------------------------------------------------- DISPLAY -----------------------------------------------------------------------------------------------------------

mov eax, BlueTextOnMagenta
call SetTextColor
call Clrscr
mov edx,offset dotline
call writeString
call crlf
mov edx,offset welcome
call writeString
call crlf
call crlf
mov edx,offset dotline
call writeString
call crlf
mov edx,offset disclaimer
call writeString
call crlf

;----------------------------------------------------------------------------------------------- PROMPTS TO TAKE USER'S INPUT -----------------------------------------------------------------------------------------------

call crlf
mov edx,offset promptEnter
call writestring
call crlf
mov edx,0
mov edx,offset inputString
mov ecx,lengthof inputString
call readstring
call crlf

;----------------------------------------------------------------------------------------- CONVERTING UPPERCASE STRING TO LOWERCASE -----------------------------------------------------------------------------------------

mov esi,offset inputString
mov ecx,lengthof inputString
lop:
	mov al,[esi]
	cmp al,65
	jb nx
	cmp al,0
	je comp
	cmp al,90
	ja nx
	add al,32
	mov [esi],al
	nx:	
		inc esi
		loop lop

;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mov edx,0
mov edx,offset promptDisplay
call writestring
call crlf
comp:
mov edx,offset inputString
call writestring
call crlf
call crlf

;------------------------------------------------------------------------------------------------- WRITING INPUT TO THE FILE ------------------------------------------------------------------------------------------------

mov edx,0
mov eax,0
mov edx, offset inputFile
call CreateOutputFile
mov lineout, al
mov esi,offset inputString
mov ecx,lengthof inputString
mov edi,offset uword
call crlf
l1:
	lodsb
	cmp al,0
	je q
	cmp al," "
	je done
	stosb
	loop l1
done:
	mov world_len,0
	mov edx,offset uword
	call writestring
	mov bl,cl
	mov ecx,lengthof uword
	mov edi,offset uword
	x:
		mov bh,[edi]
		cmp bh,0
		je d
		inc world_len
		inc edi
		loop x
	d:
		movzx eax,lineout
		mov edx, OFFSET uword
		movzx ecx,world_len
		call WriteToFile
		lea edx,new_line
		movzx eax,lineout
		mov ecx, 2
		call WriteToFile
		movzx ecx,world_len
		mov edi,offset uword
	y:
		mov bh,0
		mov [edi],bh
		inc edi
		loop y
	mov cl,bl
	mov edi,offset uword
	call crlf
	jmp l1
q:
	mov world_len,0
	mov edx,offset uword
	call writestring
	call crlf
	mov bl,cl
	mov ecx,lengthof uword
	mov edi,offset uword
	a:
		mov bh,[edi]
		cmp bh,0
		je b
		inc world_len
		inc edi
		loop a
	b:
		movzx eax,lineout
		movzx ecx,world_len
		mov edx, OFFSET uword
		call WriteToFile
		lea edx,new_line
		movzx eax,lineout
		mov ecx, 2
		call WriteToFile
		movzx eax,lineout
		mov edx, OFFSET dot
		mov ecx,1
		call WriteToFile
		mov edi,offset uword
		movzx ecx,world_len
	z:
		mov bh,0
		mov [edi],bh
		inc edi
		loop z
movzx eax, lineout
call CloseFile

;-------------------------------------------------------------------------------------- READING FROM THE FILE "input.txt" TO inputString ------------------------------------------------------------------------------------- 

mov eax, offset EmotionWordsCount
mov IndexNoEmotionWords, eax      

mov edx, offset inputFile

call openInputFile
mov inputFilehandler, eax

cmp eax, INVALID_HANDLE_VALUE
je InputFileNotExist


mov ecx, lengthOf inputString
mov edx, offset inputString
mov eax, inputfilehandler
call readFromFile
;call writeString


;------------------------------------------------------------------------------------------- FILE TEXT MOVED TO inputString---------------------------------------------------------------------------------------------------


;----------------------------------------------------------------------------------------------- CHECKING FILE'S FORMAT ------------------------------------------------------------------------------------------------------


INVOKE wordFind, addr inputString, addr dot, lengthof inputString, lengthof dot

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cmp ebx,-1
je noInputFormat
mov ebx,0


mov TempFileNames, offset FileNames
mov ecx,EmotionNum
OuterLoop:

			mov EmotionNum, ecx

			;Setting flags to zero
			mov eax, 0
			mov fileEmotionWritten, eax
			mov lastWord, eax
			mov inputFileEnded, eax

			;Resetting currentInputIndex
			mov esi, offset inputString
			mov currentInputIndex, esi 

 
			INVOKE clear, offset EmotionString, EmotionStringSize

			mov edx, TempFileNames
			call openInputFile
			mov EmotionfileHandler, eax

			cmp eax, INVALID_HANDLE_VALUE
			je FileNotExist


			mov ecx, lengthOf EmotionString
			mov edx, offset EmotionString
			mov eax, EmotionfileHandler
			call readFromFile
			;reading completed, text moved to categoryString
		


			mov ecx, mainLoopCounter
			innerLoop:
					mov mainLoopCounter, ecx
					mov eax, inputFileEnded
					cmp eax, 0
					jne breakLoopCateg2

					call extracted_Word

					INVOKE wordFind, addr EmotionString, addr extractedWord, EmotionStringSize, extractedWordSize
	
					cmp ebx, -1
					je loopEnd
					
					mov eax, fileEmotionWritten
					cmp eax, 0
					jne alreadyPrintedCateg2
					INVOKE printEmotion, TempFileNames, EmotionLength	

			alreadyPrintedCateg2:
					 INVOKE printFoundWord, offset extractedWord
					 mov EC,cl
					 INVOKE countEmotions,EmotionNum

			jmp loopEnd
					
loopEnd:
					;//restoring ecx after a function call
					mov ecx, mainLoopCounter
			loop Innerloop

			breakLoopCateg2:
			call crlf
			
	

			
			jmp skipThis
FileNotExist:
			INVOKE NoFile, tempFileNames
skipThis:

			add TempFileNames,20
			mov ecx,EmotionNum
			dec ecx
			cmp ecx, 0
jnz outerLoop



;-------------------------------------------------------------------------------------------------- PROMPTS REGARDING INPUT --------------------------------------------------------------------------------------------------
jmp skipDownStatement
noInputFormat:
	call crlf
	mov edx, offset promptNoDot
	call writeString
	call crlf

jmp skipDownStatement

InputFileNotExist:
	INVOKE NoFile, addr inputFile

skipDownStatement:

mov al, EmotionWordsCount[0]
cmp al, 0
;je exitTheProgram

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mov ecx,lengthOf EmotionCount
mov esi,0
LEC:
	mov al,EmotionCount[esi]
	cmp al,largest
	jg storeLargest
	jmp next

	storeLargest:
				mov largest,al
				mov position,esi
				jmp next

	next:
		 inc esi
Loop LEC

mov eax,0	  
mov eax,position
cmp eax,5
je pHappy
cmp eax,4
je pSad
cmp eax,3
je pAnger
cmp eax,2
je pDisgust
cmp eax,1
je pFear
cmp eax,0
je pNone

pHappy:
		mov edx,OFFSET promptHappy
		call writeString
		call crlf
		jmp conclude

pSad:
      mov edx,OFFSET promptSad
	  call writeString
	  call crlf
	  jmp conclude

pAnger:
        mov edx,OFFSET promptAnger
	    call writeString
	    call crlf
	    jmp conclude

pDisgust:
         mov edx,OFFSET promptDisgust
	     call writeString
	     call crlf
	     jmp conclude

pFear:
	   mov edx,OFFSET promptFear
	   call writeString
	   call crlf
	   jmp conclude

pNone:
	   mov edx,OFFSET promptNone
	   call writeString
	   call crlf
	   jmp conclude	   

conclude:
exit
main ENDP 
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

NoFile PROC, fileName:PTR BYTE

call crlf
call crlf
mov edx, offset promptFile1
call writeString
mov edx, fileName
call writeString
mov edx, offset promptFile2
call writeString
call crlf
call crlf

ret
NoFile ENDP

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


clear PROC, textString:PTR BYTE, StringLength:DWORD

mov edi, textString
mov eax, 0
mov ecx, stringLength
rep stosb

ret
clear ENDP

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
printFoundWord PROC, foundWord:PTR BYTE

inc foundword
mov edx, foundword
call writeString

ret
printFoundWord ENDP

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

printEmotion PROC, EmotionWord:PTR BYTE, EmotionWordSize:DWORD

mov esi, EmotionWord
mov ecx, EmotionWordSize
mov eax,0
mov fileEmotionWritten, eax

loopPrintCategName:
				mov al, [esi]
				cmp al, '.'
				je breakPrintCategName
				mov al, [esi]
				call writeChar
			
				inc esi
loop loopPrintCategName

breakPrintCategName:
mov edx, offset semiColon
call writeString
call crlf

mov eax, 0fh
mov fileEmotionWritten, eax


mov edx, offset bigSpace
call writeString

ret
printEmotion ENDP

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


extracted_Word PROC

INVOKE clear, addr extractedWord, extractedWordSize 
mov ecx, lengthOf inputString

mov eax, 0
mov ebx, 0
mov extractedWordSize, eax

mov esi, currentInputIndex
mov edi, offset extractedWord

mov al, 0ah
stosb
inc extractedWordSize


cmp al, '.'
je return

noComma:
copy:
		 mov al, [esi]
		 cmp al, 0ah
		 je addComma
		 				 
		 mov bl, [esi]
		 cmp bl, '.'
		 je FileEnded

		 movsb
		 inc extractedWordSize
loop copy

FileEnded:
mov eax, 0fh
mov inputFileEnded, eax

addComma:
mov al, 0ah
stosb
inc esi
inc extractedWordSize



return:		  ;///you need to find size of the word here	
mov currentInputIndex, esi 		        ;///preserve esi for next use, if there's a problem 

ret
extracted_Word ENDP

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

wordFind proc uses ecx esi edi eax, src: ptr byte, key: ptr byte, strSize: dword, keySize: dword

  mov ecx, strSize
  mov esi, src
  mov edi, key
  mov eax, 0
  ;dec keySize //because there is no null character
  L2:
  
    cmp eax, keySize
    jz L5

    cmpsb
    jz L3
    mov edi, key
    cmp eax, 1
    jb L4
    dec esi
    mov eax, 0
    jmp L4

    L3: 
    inc eax
   
  L4: 
  loop L2
 
  ;not found
  mov ebx, -1
  ret

  L5: ;found
      mov ebx, esi
      sub ebx, src
      sub ebx, eax
  ret
wordFind endp
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
countEmotions PROC,NoOfEmotions:DWORD

mov eax,NoOfEmotions
dec eax
cmp al,5
je inHappy
cmp al,4
je inSad
cmp al,3
je inAnger
cmp al,2
je inDisgust
cmp al,1
je inFear
cmp al,0
je inNone

inHappy:
		mov esi,eax
		add EmotionCount[si],1
		jmp last
inSad:
	  mov esi,eax
	  add EmotionCount[si],1
	  jmp last
inAnger:
	    mov esi,eax
	    add EmotionCount[si],1
		jmp last
inDisgust:
	     mov esi,eax
	     add EmotionCount[si],1
		 jmp last
inFear:
	   mov esi,eax
	   add EmotionCount[si],1
	   jmp last
inNone:
		mov esi,eax
		add EmotionCount[si],1
		jmp last

last:
ret
countEmotions ENDP
END main

;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX