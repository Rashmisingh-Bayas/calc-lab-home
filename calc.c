// new Rashmisingh Bayas 241ADB026 
// (optional) https://github.com/Rashmisingh-Bayas/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used:
// AI (DeepSeek): prompt "how do I parse -d and -o flags in C argv", conversation
// https://chat.deepseek.com/share/8e2en087xomatrxiim


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>

//path sizes
#define PATH_SIZE 512
#define NAME_SIZE 256

//this is a token, which is a number, a an operator, a bracket and output path.
typedef enum {
    NUM, PLUS, MINUS, MUL, DIV, POW, LPAREN, RPAREN, END, INVALID
} TokenType;
//in order to show errors, character position and to remember where a token starts
typedef struct {
    TokenType type;
    int is_float;
    long long i_val;
    double f_val;
    int pos;
} Token;

typedef struct {
    int error;
    int error_pos;
    int is_float;
    long long i_val;
    double f_val;
} Result;
//a shared parser state is introduced
static char *file_data;
static size_t file_len;
static size_t current_pos;
static Token current_token;

static void next_token(void);
static Result parse_expr(void);
static Result parse_term(void);
static Result parse_power(void);
static Result parse_primary(void);
static int process_file(const char *input_path, const char *output_path);
//results are stored in one helper to keep error results consistent
static Result make_error(int position)
{
    Result result = {0};
    result.error = 1;
    result.error_pos = position;
    return result;
}

static void next_token(void)
{
    while (current_pos < file_len) {
        char c = file_data[current_pos];
        if (c == ' ' || c == '\t' || c == '\r') {
            current_pos++;
        } else {
            break;
        }
    }
//the parcel has reached the end of line as no inputs are available
    if (current_pos >= file_len) {
        current_token.type = END;
        current_token.pos = (int)file_len + 1;
        return;
    }

    char c = file_data[current_pos];
    // Positions are 1-based to match the spec, and count every character.
    // current_pos is a 0-based array index, so we add 1.
    current_token.pos = (int)current_pos + 1;

    if (c == '\n') {
        current_token.type = END;
        return;
    }

    if (c == '+') current_token.type = PLUS;
    else if (c == '-') current_token.type = MINUS;
    else if (c == '/') current_token.type = DIV;
    else if (c == '(') current_token.type = LPAREN;
    else if (c == ')') current_token.type = RPAREN;
    else if (c == '*') {
        if (current_pos + 1 < file_len && file_data[current_pos + 1] == '*') {
            current_token.type = POW;
            current_pos += 2;
            return;
        }
        current_token.type = MUL;
    } else if (isdigit((unsigned char)c)) {
        size_t start = current_pos;
        int has_dot = 0;

        while (current_pos < file_len) {
            char digit = file_data[current_pos];
            if (isdigit((unsigned char)digit)) {
                current_pos++;
            } else if (digit == '.' && !has_dot) {
                has_dot = 1;
                current_pos++;
            } else {
                break;
            }
        }

        // AI (DeepSeek): prompt "how to check for trailing dot in float parsing in C", conversation https://chat.deepseek.com/share/8e2en087xomatrxiim
        // A number ending in '.' is invalid per spec. The error is reported at the '.'.
        if (has_dot && file_data[current_pos - 1] == '.') {
            current_token.pos = (int)current_pos;
            current_token.type = INVALID;
            return;
        }

        size_t length = current_pos - start;
        char number[128];
        if (length >= sizeof(number)) {
            current_token.type = INVALID;
            return;
        }

        memcpy(number, file_data + start, length);
        number[length] = '\0';

        current_token.type = NUM;
        current_token.is_float = has_dot;
        if (has_dot) current_token.f_val = atof(number);
        else current_token.i_val = atoll(number);
        return;
    } else {
        current_token.type = INVALID;
        return;
    }

    current_pos++;
}

static Result parse_primary(void)
{
    Result result = {0};

    if (current_token.type == NUM) {
        result.is_float = current_token.is_float;
        if (result.is_float) result.f_val = current_token.f_val;
        else result.i_val = current_token.i_val;
        next_token();
    } else if (current_token.type == LPAREN) {
        next_token();
        result = parse_expr();
        if (result.error) return result;

        if (current_token.type != RPAREN) {
            return make_error(current_token.pos);
        }
        next_token();
    } else {
        // No unary operators: -5 and 2 * -3 error at the '-' because primary only accepts a number or '('.
        return make_error(current_token.pos);
    }

    return result;
}

static Result parse_power(void)
{
    Result base = parse_primary();
    if (base.error) return base;

    if (current_token.type != POW) return base;

    int operator_pos = current_token.pos;
    next_token();
    int exponent_pos = current_token.pos;

    // power() calls itself for the exponent, so 2**3**2 groups as 2**(3**2).
    // This makes ** right-associative.
    Result exponent = parse_power();
    if (exponent.error) return exponent;

    // Negative integer exponent is reported at the start of the exponent per spec.
    if (!exponent.is_float && exponent.i_val < 0) {
        return make_error(exponent_pos);
    }

    Result result = {0};
    if (base.is_float || exponent.is_float) {
        double b = base.is_float ? base.f_val : (double)base.i_val;
        double e = exponent.is_float ? exponent.f_val : (double)exponent.i_val;
        result.is_float = 1;
        result.f_val = pow(b, e);

        // Float result that is infinite or NaN is reported at the ** operator.
        if (isinf(result.f_val) || isnan(result.f_val)) {
            return make_error(operator_pos);
        }
    } else {
        long long answer = 1;
        long long b = base.i_val;
        long long e = exponent.i_val;

        while (e > 0) {
            if (e % 2 == 1) answer *= b;
            b *= b;
            e /= 2;
        }
        result.i_val = answer;
    }

    return result;
}

static Result parse_term(void)
{
    Result left = parse_power();
    if (left.error) return left;

    while (current_token.type == MUL || current_token.type == DIV) {
        TokenType operator_type = current_token.type;
        int operator_pos = current_token.pos;
        next_token();
        int right_pos = current_token.pos;

        Result right = parse_power();
        if (right.error) return right;

        // Division by zero is reported at the start of the right operand per spec.
        if (operator_type == DIV &&
            ((!right.is_float && right.i_val == 0) ||
             (right.is_float && right.f_val == 0.0))) {
            return make_error(right_pos);
        }

        Result result = {0};
        if (left.is_float || right.is_float) {
            // If either operand is a float, do the operation in double.
            double l = left.is_float ? left.f_val : (double)left.i_val;
            double r = right.is_float ? right.f_val : (double)right.i_val;
            result.is_float = 1;
            result.f_val = operator_type == MUL ? l * r : l / r;

            if (isinf(result.f_val) || isnan(result.f_val)) {
                return make_error(operator_pos);
            }
        } else if (operator_type == MUL) {
            result.i_val = left.i_val * right.i_val;
        } else {
            result.i_val = left.i_val / right.i_val;
        }

        left = result;
    }

    return left;
}
//addition and subtraction is calculated here is also a top level of expression parser
static Result parse_expr(void)
{
    // expr parses the lowest precedence (+ and -). It calls term for its operands,
    // which in turn calls power. This grammar structure ensures that * and / bind
    // tighter than + and -, and ** binds tighter than everything.
    Result left = parse_term();
    if (left.error) return left;

    while (current_token.type == PLUS || current_token.type == MINUS) {
        TokenType operator_type = current_token.type;
        int operator_pos = current_token.pos;
        next_token();

        Result right = parse_term();
        if (right.error) return right;

        Result result = {0};
        if (left.is_float || right.is_float) {
            // If either operand is a float, do the operation in double.
            double l = left.is_float ? left.f_val : (double)left.i_val;
            double r = right.is_float ? right.f_val : (double)right.i_val;
            result.is_float = 1;
            result.f_val = operator_type == PLUS ? l + r : l - r;

            if (isinf(result.f_val) || isnan(result.f_val)) {
                return make_error(operator_pos);
            }
        } else if (operator_type == PLUS) {
            result.i_val = left.i_val + right.i_val;
        } else {
            result.i_val = left.i_val - right.i_val;
        }

        left = result;
    }

    return left;
}
//this tops at the first error and read expressions from one file and write each answer.
static int process_file(const char *input_path, const char *output_path)
{
    FILE *input = fopen(input_path, "rb");
    if (input == NULL) {
        fprintf(stderr, "Error: Cannot open %s\n", input_path);
        return 1;
    }

    if (fseek(input, 0, SEEK_END) != 0) {
        fprintf(stderr, "Error: Cannot read %s\n", input_path);
        fclose(input);
        return 1;
    }

    long size = ftell(input);
    if (size < 0 || fseek(input, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Error: Cannot read %s\n", input_path);
        fclose(input);
        return 1;
    }

    // Read the whole file once into memory, then walk an index over it.
    // No copies are made during parsing, so multi-megabyte files parse in O(n).
    file_len = (size_t)size;
    file_data = malloc(file_len + 1);
    if (file_data == NULL) {
        fprintf(stderr, "Error: Not enough memory to read %s\n", input_path);
        fclose(input);
        return 1;
    }

    size_t bytes_read = fread(file_data, 1, file_len, input);
    fclose(input);
    if (bytes_read != file_len) {
        fprintf(stderr, "Error: Cannot read %s\n", input_path);
        free(file_data);
        file_data = NULL;
        return 1;
    }
    file_data[file_len] = '\0';

    FILE *output = fopen(output_path, "w");
    if (output == NULL) {
        fprintf(stderr, "Error: Cannot write to %s\n", output_path);
        free(file_data);
        file_data = NULL;
        return 1;
    }

    int any_error = 0;
    int first_error_pos = 0;

    current_pos = 0;
    while (current_pos < file_len) {
        while (current_pos < file_len &&
               (file_data[current_pos] == ' ' || file_data[current_pos] == '\t' ||
                file_data[current_pos] == '\r' || file_data[current_pos] == '\n')) {
            current_pos++;
        }
        if (current_pos >= file_len) break;

        if (file_data[current_pos] == '#') {
            while (current_pos < file_len && file_data[current_pos] != '\n') {
                current_pos++;
            }
            continue;
        }

        next_token();
        if (current_token.type == INVALID) {
            any_error = 1;
            first_error_pos = current_token.pos;
            break;
        }

        Result result = parse_expr();
        if (result.error) {
            any_error = 1;
            first_error_pos = result.error_pos;
            break;
        }

        if (current_token.type != END) {
            any_error = 1;
            first_error_pos = current_token.pos;
            break;
        }

        if (result.is_float) fprintf(output, "%.15g\n", result.f_val);
        else fprintf(output, "%lld\n", result.i_val);

        while (current_pos < file_len && file_data[current_pos] != '\n') {
            current_pos++;
        }
        if (current_pos < file_len) current_pos++;
    }

    if (any_error) {
        fclose(output);
        output = fopen(output_path, "w");
        if (output == NULL) {
            fprintf(stderr, "Error: Cannot write to %s\n", output_path);
            free(file_data);
            file_data = NULL;
            return 1;
        }
        fprintf(output, "ERROR:%d\n", first_error_pos);
    }

    fclose(output);
    free(file_data);
    file_data = NULL;
    return 0;
}

// AI (DeepSeek): prompt "how to build file paths safely in C using snprintf", conversation https://chat.deepseek.com/share/8e2en087xomatrxiim
static int make_output_path(char *output_path, size_t output_size,
                            const char *output_dir, const char *base_name)
{
    int written = snprintf(output_path, output_size, "%s/%s_rashmisingh_bayas_241ADB026.txt",
                           output_dir, base_name);
    return written >= 0 && (size_t)written < output_size;
}

// Get the filename part of a path and remove its extension.
// Returns 1 when the name fits in the buffer, or 0 if it is too long.
static int get_base_name(const char *path, char *base, size_t base_size)
{
    const char *name = strrchr(path, '/');
    name = (name == NULL) ? path : name + 1;

    size_t length = strlen(name);
    if (length >= base_size) {
        return 0;
    }

    memcpy(base, name, length + 1);

    char *dot = strrchr(base, '.');
    if (dot != NULL) {
        *dot = '\0';
    }

    return 1;
}

int main(int argc, char *argv[])
{
    const char *input_file = NULL;
    const char *input_dir = NULL;
    char *output_dir = NULL;

    // AI (DeepSeek): prompt "how do I parse -d and -o flags in C argv", conversation https://chat.deepseek.com/share/8e2en087xomatrxiim
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 < argc) input_dir = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 < argc) output_dir = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: Unknown option: %s\n", argv[i]);
            return 1;
        } else {
            input_file = argv[i];
        }
    }

    if (input_file == NULL && input_dir == NULL) {
        fprintf(stderr, "Usage: calc [-d DIR] [-o OUTDIR] [input.txt]\n");
        return 1;
    }

    char default_output[NAME_SIZE];
    int exit_status = 0;
    if (input_file != NULL) {
        // Work with just the input filename, without its folder or extension.
        char base[NAME_SIZE];
        if (!get_base_name(input_file, base, sizeof(base))) {
            fprintf(stderr, "Error: Input file name is too long\n");
            return 1;
        }

        if (output_dir == NULL) {
            int written = snprintf(default_output, sizeof(default_output),
                                   "%s_rashmisingh_241ADB026", base);
            if (written < 0 || (size_t)written >= sizeof(default_output)) {
                fprintf(stderr, "Error: Output directory name is too long\n");
                return 1;
            }
            output_dir = default_output;
        }
        mkdir(output_dir, 0777);

        char output_path[PATH_SIZE];
        if (!make_output_path(output_path, sizeof(output_path), output_dir, base)) {
            fprintf(stderr, "Error: Output path is too long\n");
            return 1;
        }
        if (process_file(input_file, output_path) != 0) {
            exit_status = 1;
        }
    } else {
        DIR *directory = opendir(input_dir);
        if (directory == NULL) {
            fprintf(stderr, "Error opening directory: %s\n", input_dir);
            return 1;
        }

        if (output_dir == NULL) {
            const char *folder_name = strrchr(input_dir, '/');
            folder_name = folder_name == NULL ? input_dir : folder_name + 1;
            int written = snprintf(default_output, sizeof(default_output),
                                   "%s_rashmisingh_241ADB026", folder_name);
            if (written < 0 || (size_t)written >= sizeof(default_output)) {
                fprintf(stderr, "Error: Output directory name is too long\n");
                closedir(directory);
                return 1;
            }
            output_dir = default_output;
        }
        mkdir(output_dir, 0777);

        struct dirent *entry;
        //reading directory one at a time
        while ((entry = readdir(directory)) != NULL) {
            if (entry->d_name[0] == '.') continue;
            if (strstr(entry->d_name, ".txt") == NULL) continue;

            char input_path[PATH_SIZE];
            int written = snprintf(input_path, sizeof(input_path), "%s/%s",
                                   input_dir, entry->d_name);
            if (written < 0 || (size_t)written >= sizeof(input_path)) {
                fprintf(stderr, "Error: Input path is too long\n");
                continue;
            }

            char base[NAME_SIZE];
            if (!get_base_name(entry->d_name, base, sizeof(base))) {
                fprintf(stderr, "Error: File name is too long: %s\n", entry->d_name);
                continue;
            }

            char output_path[PATH_SIZE];
            if (!make_output_path(output_path, sizeof(output_path), output_dir, base)) {
                fprintf(stderr, "Error: Output path is too long\n");
                continue;
            }
            if (process_file(input_path, output_path) != 0) {
                exit_status = 1;
            }
        }

        closedir(directory);
    }

    return exit_status;
}