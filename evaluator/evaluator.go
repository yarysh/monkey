package evaluator

import (
	"github.com/yarysh/monkey/ast"
	"github.com/yarysh/monkey/object"
)

var (
	NULL  = &object.Null{}
	TRUE  = &object.Boolean{Value: true}
	FALSE = &object.Boolean{Value: false}
)

func Eval(node ast.Node) object.Object {
	switch node := node.(type) {

	case *ast.Program:
		return evalStatements(node.Statements)
	case *ast.ExpressionStatement:
		return Eval(node.Expression)
	case *ast.Boolean:
		return nativeBooleanObject(node.Value)
	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}
	case *ast.InfixExpression:
		left := Eval(node.Left)
		right := Eval(node.Right)
		return evalInfixExpression(node.Operator, left, right)
	case *ast.PrefixExpression:
		right := Eval(node.Right)
		return evalPrefixExpression(node.Operator, right)
	}

	return nil
}

func evalInfixExpression(operator string, left, right object.Object) object.Object {
	switch {
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ:
		return evalIntegerInfixExpression(operator, left, right)
	case operator == "==":
		return nativeBooleanObject(left == right)
	case operator == "!=":
		return nativeBooleanObject(left != right)
	default:
		return NULL
	}
}

func evalIntegerInfixExpression(operator string, left, right object.Object) object.Object {
	leftVal := left.(*object.Integer).Value
	rightVal := right.(*object.Integer).Value

	switch operator {
	case "+":
		return &object.Integer{Value: leftVal + rightVal}
	case "-":
		return &object.Integer{Value: leftVal - rightVal}
	case "*":
		return &object.Integer{Value: leftVal * rightVal}
	case "/":
		return &object.Integer{Value: leftVal / rightVal}
	case "<":
		return nativeBooleanObject(leftVal < rightVal)
	case ">":
		return nativeBooleanObject(leftVal > rightVal)
	case "==":
		return nativeBooleanObject(leftVal == rightVal)
	case "!=":
		return nativeBooleanObject(leftVal != rightVal)
	default:
		return NULL
	}
}

func evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "!":
		return evalBangOperatorExpression(right)
	case "-":
		return evalMinusPrefixOperatorExpression(right)
	}

	return nil
}

func evalBangOperatorExpression(right object.Object) object.Object {
	switch right {
	case TRUE:
		return FALSE
	case FALSE:
		return TRUE
	case NULL:
		return TRUE
	}

	return FALSE
}

func evalMinusPrefixOperatorExpression(right object.Object) object.Object {
	if right.Type() != object.INTEGER_OBJ {
		return nil
	}

	value := right.(*object.Integer).Value
	return &object.Integer{Value: -value}
}

func evalStatements(stmts []ast.Statement) object.Object {
	var result object.Object

	for _, statement := range stmts {
		result = Eval(statement)
	}

	return result
}

func nativeBooleanObject(input bool) *object.Boolean {
	if input {
		return TRUE
	}
	return FALSE
}