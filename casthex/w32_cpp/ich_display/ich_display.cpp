// ich_display.cpp : Display iching hexagrams (or pairs of them) based on castings.
//

#include "stdafx.h"


int main()
{
	char buf[20];
	static char *yin =  u8"\u2584\u2584\u2584  \u2584\u2584\u2584";
	static char *yang = u8"\u2584\u2584\u2584\u2584\u2584\u2584\u2584\u2584";
	static char *names[] = {
#include "hexnames.h"
	};

	/* determine if the stdin handle is the console */
	HANDLE inh = GetStdHandle(STD_INPUT_HANDLE);
	if (inh == NULL) return 1;
	bool is_console = (GetFileType(inh) == FILE_TYPE_CHAR);

	DWORD bytesCount;

	/* loop as long as there is input */
	while (1) {
		if (is_console) {
			printf("What are the cast lines (specify as [6-9]{6})? ");
		}
		ReadFile(inh, buf, sizeof(buf), &bytesCount, NULL);

		if (bytesCount < 6) break;
		buf[6] = '\0';

		int hex1 = 0;
		int hex2 = 0;
		char *draw1[6];
		char *draw2[6];

		/* decode the input numbers */
		for (int i = 5; i >= 0; i--) {
			hex1 <<= 1;
			hex2 <<= 1;
			switch (buf[i]) {
			case '6':  draw1[i] = yin; draw2[i] = yang;
				hex2 |= 1;
				break;
			case '7':  draw1[i] = yang; draw2[i] = yang;
				hex1 |= 1; hex2 |= 1;
				break;
			case '8':  draw1[i] = yin; draw2[i] = yin;
				break;
			case '9':  draw1[i] = yang; draw2[i] = yin;
				hex1 |= 1;
				break;
			default:
				draw1[i] = yin; draw2[i] = yin;
				fprintf(stderr, "Bad character <%c>!\n", buf[i]);
				buf[i] = '8';
			}
		}

		/* display the hexagram */
		printf("Casting for <%s>\n", buf);
		printf("\nHexagram %s\n", names[hex1]);
		if (hex1 != hex2) {
			printf(" - Changing to -\nHexagram %s\n", names[hex2]);
		}
		puts("");
		for (int i = 5; i >= 0; i--) {
			printf("%s   %s   %s\n",
				draw1[i],
				(draw1[i] == draw2[i]) ? "   " : "-->",
				(hex1 == hex2) ? "" : draw2[i]);
		}
		puts("\n\n");
	}
    return 0;
}

