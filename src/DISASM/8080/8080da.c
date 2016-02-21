// 8080 disasm.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

/*    
*codebuffer is a valid pointer to 8080 assembly code    
pc is the current offset into the code
returns the number of bytes of the op    
*/    

int Disassemble8080Op(unsigned char *codebuffer, int pc) {
	unsigned char *code = &codebuffer[pc];    
	int opbytes = 1;    
	printf ("%04x ", pc);    
	switch (*code)    
	{    
	case 0x00: printf("NOP"); break;    
	case 0x01: printf("LXI    BX,D16 #$%02x%02x", code[2], code[1]); opbytes=3; break;	//S:3	F:	B<-byte 3, C<-byte 2
	case 0x02: printf("STAX   B"); break;	//S:1	F:	(BC) <- A
	case 0x03: printf("INX    B"); break;	//S:1	F:	BC <- BC + 1
	case 0x04: printf("INR    B"); break;	//S:1	F:Z,S,P,AC	B <- B + 1
	case 0x05: printf("DCR    B"); break;	//S:1	F:Z,S,P,AC	B <- B - 1
	case 0x06: printf("MVI    B,#$%02x", code[1]); opbytes=2; break;	//S:2	F:	B <- byte 2
	case 0x07: printf("RLC"); break;	//S:1	F:CY	A = A <<1;bit 0 = prev bit 7;CY = prev bit 7
	case 0x08: printf("Unused OpC (NOP)"); break;
	case 0x09: printf("DAD B"); break;	//S:1	F:CY	HL = HL + BC
	case 0x0a: printf("LDAX B"); break;	//S:1	F:	A <- (BC)
	case 0x0b: printf("DCX B"); break;	//S:1	F:	BC = BC - 1
	case 0x0c: printf("INR C"); break;	//S:1	F:Z,S,p,AC	C <- C + 1
	case 0x0d: printf("INR D"); break;	//S:1	F:Z,S,p,AC	C <- C - 1
	case 0x0e: printf("MVI C,D8 #$%02x", code[1]); opbytes=2; break;	//S:2	F:	C <- byte 2
	case 0x0f: printf("RRC"); break;	//S:1	F:CY	A = A >> 1;bit 7 = prev bit 0;CY = prev bit 0
	case 0x10: printf("Unused OpC (NOP)"); break;
	case 0x11: printf("LXI D,D16 #$%02x%02x", code[2], code[1]); opbytes=3; break;	//S:3	F:	D<-byte 3, E<-byte 2
	case 0x12: printf("STAX D"); break;	//S:1	F:	(DE) <- A
	case 0x13: printf("INX D"); break;	//S:1	F:	DE <- DE + 1
	case 0x14: printf("INR D"); break;	//S:1	F:Z,S,P,AC	D <- D + 1
	case 0x15: printf("DCR D"); break;	//S:1	F:Z,S,P,AC	D <- D - 1
	case 0x16: printf("MVI D,D8 #$%02x", code[1]); opbytes=2; break;	//S:2	F:	D <- byte 2
	case 0x17: printf("RAL"); break;	//S:1	F:CY	A = A << 1;bit 0 = prev CY;CY = prev bit 7
	case 0x18: printf("Unused OpC (NOP)"); break;	//
	case 0x19: printf("DAD D"); break;	//S:1	F:CY	HL = HL + DE
	case 0x1a: printf("LDAX DX"); break;	//S:1	F:	A <- (DE)
	case 0x1b: printf("DCX DX"); break;	//S:1	F:	DE = DE - 1
	case 0x1c: printf("INR E"); break;	//S:1	F:Z,S,P,AC	E = E + 1
	case 0x1d: printf("DCR E"); break;	//S:1	F:Z,S,P,AC	E = E - 1
	case 0x1e: printf("MVI E,D8"); opbytes=2; break;	//S:2	F:	E <- byte 2
	case 0x1f: printf("RAR"); break;	//S:1	F:CY	A = A >> 1;bit 7 = prev bit 7;CY = prev bit 0
	case 0x20: printf("RIM"); break;	//S:1	F:	special - co to znaci???
	case 0x21: printf("LXI H,D16 #$%02x%02x", code[2], code[1]); opbytes=3; break;	//S:3 F:	H <- byte 3, L <- byte 2"
	case 0x22: printf("SHLD adr (0x%02X%02X)", code[2], code[1]); opbytes=3; break;	//S:3 F:	(adr) <-L; (adr+1)<-H"
	case 0x23: printf("INX H"); break;	//S:1 F:	HL <- HL + 1"
	case 0x24: printf("INR H"); break;	//S:1 F:Z, S, P, AC	H <- H+1"
	case 0x25: printf("DCR H"); break;	//S:1 F:Z, S, P, AC	H <- H-1"
	case 0x26: printf("MVI H,D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:	H <- byte 2"
	case 0x27: printf("DAA"); break;	//S:1 F:	special"
	case 0x28: printf("Unused OpC (NOP)"); break;	//S:1 F:
	case 0x29: printf("DAD H"); break;	//S:1 F:CY	HL = HL + HI"
	case 0x2a: printf("LHLD adr (0x%02X%02X)", code[2], code[1]); opbytes=3; break;	//S:3 F:	L <- (adr); H<-(adr+1)"
	case 0x2b: printf("DCX H"); break;	//S:1 F:	HL = HL-1"
	case 0x2c: printf("INR L"); break;	//S:1 F:Z, S, P, AC	L <- L+1"
	case 0x2d: printf("DCR L"); break;	//S:1 F:Z, S, P, AC	L <- L-1"
	case 0x2e: printf("MVI L, D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:	L <- byte 2"
	case 0x2f: printf("CMA"); break;	//S:1 F:	A <- !A"
	case 0x30: printf("SIM"); break;	//S:1 F:	special"
	case 0x31: printf("LXI SP, D16 #$%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	SP.hi <- byte 3, SP.lo <- byte 2"
	case 0x32: printf("STA adr $%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	(adr) <- A"
	case 0x33: printf("INX SP"); break;	//S:1 F:	SP = SP + 1"
	case 0x34: printf("INR M"); break;	//S:1 F:Z, S, P, AC	(HL) <- (HL)+1"
	case 0x35: printf("DCR M"); break;	//S:1 F:Z, S, P, AC	(HL) <- (HL)-1"
	case 0x36: printf("MVI M,D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:	(HL) <- byte 2"
	case 0x37: printf("STC"); break;	//S:1 F:CY	CY = 1"
	case 0x38: printf("Unused OpC (NOP)"); break;	//S:1 F:
	case 0x39: printf("DAD SP"); break;	//S:1 F:CY	HL = HL + SP"
	case 0x3a: printf("LDA adr (0x%02X%02X)", code[2], code[1]); opbytes=3; break;	//S:3 F:	A <- (adr)"
	case 0x3b: printf("DCX SP"); break;	//S:1 F:	SP = SP-1"
	case 0x3c: printf("INR A"); break;	//S:1 F:Z, S, P, AC	A <- A+1"
	case 0x3d: printf("DCR A"); break;	//S:1 F:Z, S, P, AC	A <- A-1"
	// ----------------------------PROJIT RUCNE - IMPORT Z EXCELU
	case 0x3e: printf("MVI A,D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:	A <- byte 2"
	case 0x3f: printf("CMC"); break;	//S:1 F:CY	CY=!CY"
	case 0x40: printf("MOV B,B"); break;	//S:1 F:	B <- B"
	case 0x41: printf("MOV B,C"); break;	//S:1 F:	B <- C"
	case 0x42: printf("MOV B,D"); break;	//S:1 F:	B <- D"
	case 0x43: printf("MOV B,E"); break;	//S:1 F:	B <- E"
	case 0x44: printf("MOV B,H"); break;	//S:1 F:	B <- H"
	case 0x45: printf("MOV B,L"); break;	//S:1 F:	B <- L"
	case 0x46: printf("MOV B,M"); break;	//S:1 F:	B <- (HL)"
	case 0x47: printf("MOV B,A"); break;	//S:1 F:	B <- A"
	case 0x48: printf("MOV C,B"); break;	//S:1 F:	C <- B"
	case 0x49: printf("MOV C,C"); break;	//S:1 F:	C <- C"
	case 0x4a: printf("MOV C,D"); break;	//S:1 F:	C <- D"
	case 0x4b: printf("MOV C,E"); break;	//S:1 F:	C <- E"
	case 0x4c: printf("MOV C,H"); break;	//S:1 F:	C <- H"
	case 0x4d: printf("MOV C,L"); break;	//S:1 F:	C <- L"
	case 0x4e: printf("MOV C,M"); break;	//S:1 F:	C <- (HL)"
	case 0x4f: printf("MOV C,A"); break;	//S:1 F:	C <- A"
	case 0x50: printf("MOV D,B"); break;	//S:1 F:	D <- B"
	case 0x51: printf("MOV D,C"); break;	//S:1 F:	D <- C"
	case 0x52: printf("MOV D,D"); break;	//S:1 F:	D <- D"
	case 0x53: printf("MOV D,E"); break;	//S:1 F:	D <- E"
	case 0x54: printf("MOV D,H"); break;	//S:1 F:	D <- H"
	case 0x55: printf("MOV D,L"); break;	//S:1 F:	D <- L"
	case 0x56: printf("MOV D,M"); break;	//S:1 F:	D <- (HL)"
	case 0x57: printf("MOV D,A"); break;	//S:1 F:	D <- A"
	case 0x58: printf("MOV E,B"); break;	//S:1 F:	E <- B"
	case 0x59: printf("MOV E,C"); break;	//S:1 F:	E <- C"
	case 0x5a: printf("MOV E,D"); break;	//S:1 F:	E <- D"
	case 0x5b: printf("MOV E,E"); break;	//S:1 F:	E <- E"
	case 0x5c: printf("MOV E,H"); break;	//S:1 F:	E <- H"
	case 0x5d: printf("MOV E,L"); break;	//S:1 F:	E <- L"
	case 0x5e: printf("MOV E,M"); break;	//S:1 F:	E <- (HL)"
	case 0x5f: printf("MOV E,A"); break;	//S:1 F:	E <- A"
	case 0x60: printf("MOV H,B"); break;	//S:1 F:	H <- B"
	case 0x61: printf("MOV H,C"); break;	//S:1 F:	H <- C"
	case 0x62: printf("MOV H,D"); break;	//S:1 F:	H <- D"
	case 0x63: printf("MOV H,E"); break;	//S:1 F:	H <- E"
	case 0x64: printf("MOV H,H"); break;	//S:1 F:	H <- H"
	case 0x65: printf("MOV H,L"); break;	//S:1 F:	H <- L"
	case 0x66: printf("MOV H,M"); break;	//S:1 F:	H <- (HL)"
	case 0x67: printf("MOV H,A"); break;	//S:1 F:	H <- A"
	case 0x68: printf("MOV L,B"); break;	//S:1 F:	L <- B"
	case 0x69: printf("MOV L,C"); break;	//S:1 F:	L <- C"
	case 0x6a: printf("MOV L,D"); break;	//S:1 F:	L <- D"
	case 0x6b: printf("MOV L,E"); break;	//S:1 F:	L <- E"
	case 0x6c: printf("MOV L,H"); break;	//S:1 F:	L <- H"
	case 0x6d: printf("MOV L,L"); break;	//S:1 F:	L <- L"
	case 0x6e: printf("MOV L,M"); break;	//S:1 F:	L <- (HL)"
	case 0x6f: printf("MOV L,A"); break;	//S:1 F:	L <- A"
	case 0x70: printf("MOV M,B"); break;	//S:1 F:	(HL) <- B"
	case 0x71: printf("MOV M,C"); break;	//S:1 F:	(HL) <- C"
	case 0x72: printf("MOV M,D"); break;	//S:1 F:	(HL) <- D"
	case 0x73: printf("MOV M,E"); break;	//S:1 F:	(HL) <- E"
	case 0x74: printf("MOV M,H"); break;	//S:1 F:	(HL) <- H"
	case 0x75: printf("MOV M,L"); break;	//S:1 F:	(HL) <- L"
	case 0x76: printf("HLT"); break;	//S:1 F:	special"
	case 0x77: printf("MOV M,A"); break;	//S:1 F:	(HL) <- C"
	case 0x78: printf("MOV A,B"); break;	//S:1 F:	A <- B"
	case 0x79: printf("MOV A,C"); break;	//S:1 F:	A <- C"
	case 0x7a: printf("MOV A,D"); break;	//S:1 F:	A <- D"
	case 0x7b: printf("MOV A,E"); break;	//S:1 F:	A <- E"
	case 0x7c: printf("MOV A,H"); break;	//S:1 F:	A <- H"
	case 0x7d: printf("MOV A,L"); break;	//S:1 F:	A <- L"
	case 0x7e: printf("MOV A,M"); break;	//S:1 F:	A <- (HL)"
	case 0x7f: printf("MOV A,A"); break;	//S:1 F:	A <- A"
	case 0x80: printf("ADD B"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + B"
	case 0x81: printf("ADD C"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + C"
	case 0x82: printf("ADD D"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + D"
	case 0x83: printf("ADD E"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + E"
	case 0x84: printf("ADD H"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + H"
	case 0x85: printf("ADD L"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + L"
	case 0x86: printf("ADD M"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + (HL)"
	case 0x87: printf("ADD A"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + A"
	case 0x88: printf("ADC B"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + B + CY"
	case 0x89: printf("ADC C"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + C + CY"
	case 0x8a: printf("ADC D"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + D + CY"
	case 0x8b: printf("ADC E"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + E + CY"
	case 0x8c: printf("ADC H"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + H + CY"
	case 0x8d: printf("ADC L"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + L + CY"
	case 0x8e: printf("ADC M"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + (HL) + CY"
	case 0x8f: printf("ADC A"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + A + CY"
	case 0x90: printf("SUB B"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - B"
	case 0x91: printf("SUB C"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - C"
	case 0x92: printf("SUB D"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + D"
	case 0x93: printf("SUB E"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - E"
	case 0x94: printf("SUB H"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + H"
	case 0x95: printf("SUB L"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - L"
	case 0x96: printf("SUB M"); break;	//S:1 F:Z, S, P, CY, AC	A <- A + (HL)"
	case 0x97: printf("SUB A"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - A"
	case 0x98: printf("SBB B"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - B - CY"
	case 0x99: printf("SBB C"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - C - CY"
	case 0x9a: printf("SBB D"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - D - CY"
	case 0x9b: printf("SBB E"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - E - CY"
	case 0x9c: printf("SBB H"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - H - CY"
	case 0x9d: printf("SBB L"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - L - CY"
	case 0x9e: printf("SBB M"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - (HL) - CY"
	case 0x9f: printf("SBB A"); break;	//S:1 F:Z, S, P, CY, AC	A <- A - A - CY"
	case 0xa0: printf("ANA B"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & B"
	case 0xa1: printf("ANA C"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & C"
	case 0xa2: printf("ANA D"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & D"
	case 0xa3: printf("ANA E"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & E"
	case 0xa4: printf("ANA H"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & H"
	case 0xa5: printf("ANA L"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & L"
	case 0xa6: printf("ANA M"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & (HL)"
	case 0xa7: printf("ANA A"); break;	//S:1 F:Z, S, P, CY, AC	A <- A & A"
	case 0xa8: printf("XRA B"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ B"
	case 0xa9: printf("XRA C"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ C"
	case 0xaa: printf("XRA D"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ D"
	case 0xab: printf("XRA E"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ E"
	case 0xac: printf("XRA H"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ H"
	case 0xad: printf("XRA L"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ L"
	case 0xae: printf("XRA M"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ (HL)"
	case 0xaf: printf("XRA A"); break;	//S:1 F:Z, S, P, CY, AC	A <- A ^ A"
	case 0xb0: printf("ORA B"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | B"
	case 0xb1: printf("ORA C"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | C"
	case 0xb2: printf("ORA D"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | D"
	case 0xb3: printf("ORA E"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | E"
	case 0xb4: printf("ORA H"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | H"
	case 0xb5: printf("ORA L"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | L"
	case 0xb6: printf("ORA M"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | (HL)"
	case 0xb7: printf("ORA A"); break;	//S:1 F:Z, S, P, CY, AC	A <- A | A"
	case 0xb8: printf("CMP B"); break;	//S:1 F:Z, S, P, CY, AC	A - B"
	case 0xb9: printf("CMP C"); break;	//S:1 F:Z, S, P, CY, AC	A - C"
	case 0xba: printf("CMP D"); break;	//S:1 F:Z, S, P, CY, AC	A - D"
	case 0xbb: printf("CMP E"); break;	//S:1 F:Z, S, P, CY, AC	A - E"
	case 0xbc: printf("CMP H"); break;	//S:1 F:Z, S, P, CY, AC	A - H"
	case 0xbd: printf("CMP L"); break;	//S:1 F:Z, S, P, CY, AC	A - L"
	case 0xbe: printf("CMP M"); break;	//S:1 F:Z, S, P, CY, AC	A - (HL)"
	case 0xbf: printf("CMP A"); break;	//S:1 F:Z, S, P, CY, AC	A - A"
	case 0xc0: printf("RNZ"); break;	//S:1 F:	if NZ, RET"
	case 0xc1: printf("POP B"); break;	//S:1 F:	C <- (sp); B <- (sp+1); sp <- sp+2"
	case 0xc2: printf("JNZ adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if NZ, PC <- adr"
	case 0xc3: printf("JMP adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	PC <= adr"
	case 0xc4: printf("CNZ adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if NZ, CALL adr"
	case 0xc5: printf("PUSH BC"); break;	//S:1 F:	(sp-2)<-C; (sp-1)<-B; sp <- sp - 2"
	case 0xc6: printf("ADI D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A <- A + byte"
	case 0xc7: printf("RST 0"); break;	//S:1 F:	CALL $0"
	case 0xc8: printf("RZ"); break;	//S:1 F:	if Z, RET"
	case 0xc9: printf("RET"); break;	//S:1 F:	PC.lo <- (sp); PC.hi<-(sp+1); SP <- SP+2"
	case 0xca: printf("JZ adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if Z, PC <- adr"
	case 0xcb: printf("-"); break;	//S: F:	"
	case 0xcc: printf("CZ adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if Z, CALL adr"
	case 0xcd: printf("CALL adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	(SP-1)<-PC.hi;(SP-2)<-PC.lo;SP<-SP+2;PC=adr"
	case 0xce: printf("ACI D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A <- A + data + CY"
	case 0xcf: printf("RST 1"); break;	//S:1 F:	CALL $8"
	case 0xd0: printf("RNC"); break;	//S:1 F:	if NCY, RET"
	case 0xd1: printf("POP D"); break;	//S:1 F:	E <- (sp); D <- (sp+1); sp <- sp+2"
	case 0xd2: printf("JNC adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if NCY, PC<-adr"
	case 0xd3: printf("OUT D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:	special"
	case 0xd4: printf("CNC adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if NCY, CALL adr"
	case 0xd5: printf("PUSH DE"); break;	//S:1 F:	(sp-2)<-E; (sp-1)<-D; sp <- sp - 2"
	case 0xd6: printf("SUI D8 #$%02x", code[2], code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A <- A - data"
	case 0xd7: printf("RST 2"); break;	//S:1 F:	CALL $10"
	case 0xd8: printf("RC"); break;	//S:1 F:	if CY, RET"
	case 0xd9: printf("-"); break;	//S: F:	"
	case 0xda: printf("JC adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if CY, PC<-adr"
	case 0xdb: printf("IN D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:	special"
	case 0xdc: printf("CC adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if CY, CALL adr"
	case 0xdd: printf("-"); break;	//S: F:	"
	case 0xde: printf("SBI D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A <- A - data - CY"
	case 0xdf: printf("RST 3"); break;	//S:1 F:	CALL $18"
	case 0xe0: printf("RPO"); break;	//S:1 F:	if PO, RET"
	case 0xe1: printf("POP H"); break;	//S:1 F:	L <- (sp); H <- (sp+1); sp <- sp+2"
	case 0xe2: printf("JPO adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if PO, PC <- adr"
	case 0xe3: printf("XTHL"); break;	//S:1 F:	L <-> (SP); H <-> (SP+1)"
	case 0xe4: printf("CPO adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if PO, CALL adr"
	case 0xe5: printf("PUSH HL"); break;	//S:1 F:	(sp-2)<-L; (sp-1)<-H; sp <- sp - 2"
	case 0xe6: printf("ANI D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A <- A & data"
	case 0xe7: printf("RST 4"); break;	//S:1 F:	CALL $20"
	case 0xe8: printf("RPE"); break;	//S:1 F:	if PE, RET"
	case 0xe9: printf("PCHL"); break;	//S:1 F:	PC.hi <- H; PC.lo <- L"
	case 0xea: printf("JPE adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if PE, PC <- adr"
	case 0xeb: printf("XCHG"); break;	//S:1 F:	H <-> D; L <-> E"
	case 0xec: printf("CPE adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if PE, CALL adr"
	case 0xed: printf("-"); break;	//S: F:	"
	case 0xee: printf("XRI D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A <- A ^ data"
	case 0xef: printf("RST 5"); break;	//S:1 F:	CALL $28"
	case 0xf0: printf("RP"); break;	//S:1 F:	if P, RET"
	case 0xf1: printf("POP PSW"); break;	//S:1 F:	flags <- (sp); A <- (sp+1); sp <- sp+2"
	case 0xf2: printf("JP adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if P=1 PC <- adr"
	case 0xf3: printf("DI"); break;	//S:1 F:	special"
	case 0xf4: printf("CP adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if P, PC <- adr"
	case 0xf5: printf("PUSH AF"); break;	//S:1 F:	(sp-2)<-flags; (sp-1)<-A; sp <- sp - 2"
	case 0xf6: printf("ORI D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A <- A | data"
	case 0xf7: printf("RST 6"); break;	//S:1 F:	CALL $30"
	case 0xf8: printf("RM"); break;	//S:1 F:	if M, RET"
	case 0xf9: printf("SPHL"); break;	//S:1 F:	SP=HL"
	case 0xfa: printf("JM adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if M, PC <- adr"
	case 0xfb: printf("EI"); break;	//S:1 F:	special"
	case 0xfc: printf("CM adr 0x%02X%02X", code[2], code[1]); opbytes=3; break;	//S:3 F:	if M, CALL adr"
	case 0xfd: printf("-"); break;	//S: F:	"
	case 0xfe: printf("CPI D8 #$%02x", code[1]); opbytes=2; break;	//S:2 F:Z, S, P, CY, AC	A - data"
	case 0xff: printf("RST 7"); break;	//S:1 F:	CALL $38"
	default: printf("Neznamy OpCode %02x",code[0]);break;
	}    

	printf("\n");    

	return opbytes;    
}

int main (int argc, char**argv)    
{    
	FILE *f= fopen(argv[1], "rb");    
	if (f==NULL)    
	{    
		printf("error: Couldn't open %s\n", argv[1]);    
		exit(1);    
	}

	//Get the file size and read it into a memory buffer    
	fseek(f, 0L, SEEK_END);    
	int fsize = ftell(f);    
	fseek(f, 0L, SEEK_SET);    

	unsigned char *buffer=(unsigned char*) malloc(fsize);    

	fread(buffer, fsize, 1, f);    
	fclose(f);    

	int pc = 0;    

	while (pc < fsize)    
	{    
		pc += Disassemble8080Op(buffer, pc);    
	}    
	return 0;    
}    
