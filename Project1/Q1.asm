TITLE My First Program (Test.asm)
INCLUDE Irvine32.inc
.code
main PROC
mov eax,7h
sub eax,3h
add eax,24h
call DumpRegs
exit
main ENDP
END main