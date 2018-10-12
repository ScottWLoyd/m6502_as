#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <assert.h>
#include <vector>

static char* replace_extension(char* str, const char* new_ext) {
	size_t orig_len = strlen(str);
	char* ptr = str;
	char* last_dot = NULL;
	for (;;) {
		if (*ptr == NULL)
		{
			break;
		}
		if (*ptr == '.')
		{
			last_dot = ptr;
		}
		ptr++;
	}
	size_t base_len = last_dot - str;
	size_t new_len = base_len + strlen(new_ext) + 1;
	char* result = (char*)malloc(new_len);
	strncpy(result, str, base_len);
	strncpy(result + base_len, new_ext, strlen(new_ext));
	result[new_len] = 0;
	return result;
}

typedef enum TokenKind {
	TokenUnknown,
	TokenInstr,
	TokenHash,
	TokenDollar,
	TokenLParen,
	TokenRParen,
	TokenByte,
	TokenWord,
	TokenReg,
	TokenComma,
	TokenEof,
} TokenKind;

static const char* tokenkind_to_str(TokenKind kind) {
	switch (kind)
	{
	case TokenUnknown: return "Unknown";
	case TokenInstr: return "Instruction";
	case TokenHash: return "Immediate";
	case TokenDollar: return "Literal";
	case TokenLParen: return "Left Parenthesis";
	case TokenRParen: return "Right Parenthesis";
	case TokenByte: return "Byte";
	case TokenWord: return "Word";
	case TokenReg: return "Register";
	case TokenComma: return "Comma";
	case TokenEof: return "EOF";
	default: assert(0);
	}
	return NULL;
}

typedef struct Tokenizer {
	const char* string;
	char* pt;
	TokenKind kind;
	char* start;
	char* end;
	uint16_t val;
	size_t line;
} Tokenizer;

static Tokenizer tokenizer;
static void init_tokenizer(const char* str) {
	tokenizer = { 0 };
	tokenizer.string = str;	
	tokenizer.pt = (char*)str;
	tokenizer.line = 1;
}

static bool is_hex(char c) {
	if ('0' <= c && c <= '9' || 
		'a' <= c && c <= 'f' ||
		'A' <= c && c <= 'F') {
		return true;
	}
	return false;
}

static uint32_t parse_hex(char* str, int num_chars)
{
	uint32_t result = 0;
	for (; num_chars > 0; str++, num_chars--) {
		char c = tolower(*str);
		uint32_t i;
		if ('a' <= c && c <= 'f') {
			i = 10 + (c - 'a');
		}
		else {
			assert('0' <= c && c <= '9');
			i = (c - '0');
		}
		result = (16 * result) + i;
	}
	return result;
}

static void next_token() {	
	while (*tokenizer.pt == ' ' || *tokenizer.pt == '\t' || *tokenizer.pt == '\v' || *tokenizer.pt == '\r' || *tokenizer.pt == '\n') {
		if (*tokenizer.pt == '\n') {
			tokenizer.line++;
		}
		tokenizer.pt++;
	}
	if (*tokenizer.pt == 0)	{
		tokenizer.kind = TokenEof;
		return;
	}

	tokenizer.start = tokenizer.pt;
	switch (*tokenizer.pt) {
	case '#': {
		tokenizer.kind = TokenHash;
		tokenizer.pt++;
		tokenizer.end = tokenizer.pt;
	} break;
	case '$': {
		tokenizer.kind = TokenDollar;
		tokenizer.pt++;
		tokenizer.end = tokenizer.pt;
	} break;
	case '(': {
		tokenizer.kind = TokenLParen;
		tokenizer.pt++;
		tokenizer.end = tokenizer.pt;
	} break;
	case ')': {
		tokenizer.kind = TokenRParen;
		tokenizer.pt++;
		tokenizer.end = tokenizer.pt;
	} break;
	case ',': {
		tokenizer.kind = TokenComma;
		tokenizer.pt++;
		tokenizer.end = tokenizer.pt;
	} break;
	default: {

		// first determine length
		while (isalnum(*tokenizer.pt)) {
			tokenizer.pt++;
		}

		size_t len = tokenizer.pt - tokenizer.start;
		if (len == 1) {
			char c = *tokenizer.start;
			if (c == 'A' || c == 'a' ||
				c == 'X' || c == 'x' ||
				c == 'Y' || c == 'y') {
				tokenizer.kind = TokenReg;
			}
			else {
				tokenizer.kind = TokenUnknown;
			}
		}
		else if (len == 2) {
			// verify that this is a valid hex string
			if (is_hex(tokenizer.start[0]) && is_hex(tokenizer.start[1])) {
				tokenizer.kind = TokenByte;
				tokenizer.val = parse_hex(tokenizer.start, 2);
			}
		}
		else if (len == 3) {
			tokenizer.kind = TokenInstr;
			tokenizer.end = tokenizer.pt;
		}
		else if (len == 4) {
			if (is_hex(tokenizer.start[0]) && is_hex(tokenizer.start[1]) &&
				is_hex(tokenizer.start[2]) && is_hex(tokenizer.start[3])) {
				tokenizer.pt += 4;
				tokenizer.kind = TokenWord;
				tokenizer.val = parse_hex(tokenizer.start, 4);
			}
		}
		else
		{
			tokenizer.kind = TokenUnknown;
		}
		tokenizer.end = tokenizer.pt;
	} break;
	} 
}

typedef enum InstrKind {
	InstrAdd,
	InstrAnd,
	InstrAsl,
	InstrBit,
	InstrBra,
	InstrBrk,
	InstrCmp,
	InstrCpx,
	InstrCpy,
	InstrDec,
	InstrEor,
	InstrFlag,
	InstrInc,
	InstrJsr,
	InstrLda,
	InstrLdx,
	InstrLdy,
	InstrLsr,
	InstrNop,
	InstrOra,
	InstrReg,
	InstrRol,
	InstrRor,
	InstrRti,
	InstrRts,
	InstrSbc,
	InstrSta,
	InstrStk,
	InstrStx,
	InstrSty,
} InstrKind;

typedef enum Mode {
	ModeAcc,
	ModeImp,
	ModeImm,
	ModeAbs,
	ModeZp,
	ModeRel,
	ModeAix,
	ModeAiy,
	ModeZpx,
	ModeZpy,
	ModeIdx,
	ModeIdy
} Mode;

typedef enum Register {
	RegA,
	RegX,
	RegY,
} Register;

typedef struct Instr {
	InstrKind kind;
	Mode mode;
	uint16_t op1;
	uint16_t op2;
	Register reg;
} Instr;

static void expect_token(TokenKind kind) {
	next_token();
	if (tokenizer.kind != kind) {
		printf("Error(line %d): expected token kind %s, found kind %s\n", 
			tokenizer.line, tokenkind_to_str(kind), tokenkind_to_str(tokenizer.kind));
		exit(1);
	}
}

static bool match_token(TokenKind kind) {
	next_token();
	return tokenizer.kind == kind;
}

static void expect_byte() {
	next_token();
	if (tokenizer.kind == TokenDollar) {
		next_token();
		if (tokenizer.kind == TokenByte) {
			return;
		}
	}
	printf("Error(line %d): expected byte; got %s\n", tokenizer.line, tokenkind_to_str(tokenizer.kind));
	exit(1);
}

static bool match_byte() {
	next_token();
	if (tokenizer.kind == TokenDollar) {
		next_token();
		if (tokenizer.kind == TokenByte) {
			return true;
		}
	}
	return false;
}

static void expect_word() {
	next_token();
	if (tokenizer.kind == TokenDollar) {
		next_token();
		if (tokenizer.kind == TokenWord) {
			return;
		}
	}
	printf("Error(line %d): expected word; got %s\n", tokenizer.line, tokenkind_to_str(tokenizer.kind));
	exit(1);
}

static bool match_word() {
	next_token();
	if (tokenizer.kind == TokenDollar) {
		next_token();
		if (tokenizer.kind == TokenWord) {
			return true;
		}
	}
	return false;
}

static Instr parse_lda() {
	Instr instr;
	instr.kind = InstrLda;
	if (match_token(TokenHash)) {
		instr.mode = ModeImm;
		if (match_byte()) {
			instr.op1 = tokenizer.val;
		}
		else {
			printf("Error(line %d): expected byte value for immediate; got '%s'\n", tokenizer.line, tokenkind_to_str(tokenizer.kind));
			exit(1);
		}
	}
	else if (match_token(TokenLParen)) {
		expect_byte();
		instr.op1 = tokenizer.val;
		if (match_token(TokenRParen)) {
			instr.mode = ModeIdy;
			expect_token(TokenComma);
			expect_token(TokenReg);
			if (*tokenizer.start != 'Y') {
				printf("Error(line %d): invalid register '%c' for LDA instruction\n", tokenizer.line, *tokenizer.start);
				exit(1);
			}
			instr.reg = RegY;
		}
		else if (match_token(TokenComma)) {
			instr.mode = ModeIdx;
			expect_token(TokenReg);
			if (*tokenizer.start != 'X') {
				printf("Error(line %d): invalid register '%c' for LDA instruction\n", tokenizer.line, *tokenizer.start);
				exit(1);
			}
			expect_token(TokenRParen);
			instr.reg = RegX;
		}
	}
	else if (match_byte()) {
		instr.op1 = tokenizer.val;
		if (match_token(TokenComma)) {
			expect_token(TokenReg);
			if (*tokenizer.start != 'X') {
				printf("Error(line %d): invalid register '%c' for LDA instruction\n", tokenizer.line, *tokenizer.start);
				exit(1);
			}
			instr.mode = ModeZpx;
		}
		else {
			instr.mode = ModeZp;
		}
	}
	else {
		expect_word();
		instr.op1 = tokenizer.val;
		if (match_token(TokenComma)) {
			expect_token(TokenReg);
			if (*tokenizer.start == 'X') {
				instr.mode = ModeAix;
			}
			else if (*tokenizer.start == 'Y') {
				instr.mode = ModeAiy;
			}
			else {
				printf("Error(line %d): invalid register '%c' for LDA instruction\n", tokenizer.line, *tokenizer.start);
				exit(1);
			}
		}
	}
	return instr;
}

static Instr parse_instruction() {
	expect_token(TokenInstr);	
	if (strncmp("LDA", tokenizer.start, 3) == 0) {
		return parse_lda();
	}

	return Instr{};
}

static void process_file(char* filename, const char* str) {
	printf("Assembling %s\n", filename);	

	init_tokenizer(str);
	std::vector<Instr> instructions;
	for (;;) {
		if (match_token(TokenEof)) {
			break;
		}
		Instr instr = parse_instruction();
		instructions.push_back(instr);
	}

	char* output_filename = replace_extension(filename, ".o");
	FILE* output_file = fopen(output_filename, "wb");
	if (!output_file) {
		printf("Error: unable to open output file '%s'\n", output_filename);
		return;
	}
	for (Instr instr : instructions)
	{
		//emit_instruction(output_file, instr);
	}
}

static void process_flag(char* flag) {

}

int main(int argc, char** argv) {
	if (argc < 2) {
		printf("Usage: %s [flags] <assembly_file>\n", argv[0]);
		return 0;
	}

	std::vector<char*> filenames;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			process_flag(argv[i]);
		}
		else {
			filenames.push_back(argv[i]);
		}
	}

	for (auto filename : filenames)	{
		FILE* file = fopen(filename, "r");
		if (!file) {
			printf("Error: failed to open input file '%s'\n", filename);
			continue;
		}

		fseek(file, 0, SEEK_END);
		long file_size = ftell(file);
		fseek(file, 0, SEEK_SET);

		char* string = (char*)malloc(file_size + 1);
		fread(string, 1, file_size, file);
		fclose(file);
		string[file_size] = 0;

		process_file(filename, string);
	}

	return 0;
}