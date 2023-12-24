package parser

import (
	"testing"

	"github.com/yarysh/monkey/ast"
	"github.com/yarysh/monkey/lexer"
)

func TestLetStatements(t *testing.T) {
	input := `
let x = 5;
let y = 10;
let foobar = 838383;
`
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	checkParserErrors(t, p)

	if got, want := len(program.Statements), 3; got != want {
		t.Fatalf("program.Statements =%d, want=%d", got, want)
	}

	tests := []struct {
		expectedIdentifier string
	}{
		{"x"},
		{"y"},
		{"foobar"},
	}

	for i, tt := range tests {
		stmt := program.Statements[i]
		if !testLetStatement(t, stmt, tt.expectedIdentifier) {
			return
		}
	}
}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf("s.TokenLiteral not 'let' . got=%q", s.TokenLiteral())
		return false
	}

	letStmt, ok := s.(*ast.LetStatement)
	if !ok {
		t.Errorf("s not *ast.LetStatement. got=%T", s)
	}

	if got, want := letStmt.Name.Value, name; got != want {
		t.Errorf("letStmt.Name.Value: got %s, want %s", name, letStmt.Name.Value)
	}

	if got, want := letStmt.Name.TokenLiteral(), name; got != want {
		t.Errorf("letStmt.Name.TokenLiteral(): got %s, want %s", name, letStmt.Name.Value)
	}

	return true
}
