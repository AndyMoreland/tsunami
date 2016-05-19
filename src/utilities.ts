import * as ts from "typescript";

/* Taken from typescript compiler because they are not exported. */

export function isClassLike(node: ts.Node): node is ts.ClassLikeDeclaration {
    return node && (node.kind === ts.SyntaxKind.ClassDeclaration || node.kind === ts.SyntaxKind.ClassExpression);
}

export function isExpressionWithTypeArgumentsInClassExtendsClause(node: ts.Node): boolean {
    return node.kind === ts.SyntaxKind.ExpressionWithTypeArguments &&
        (<ts.HeritageClause>node.parent).token === ts.SyntaxKind.ExtendsKeyword &&
        isClassLike(node.parent.parent);
}

export function isExpression(node: ts.Node): boolean {
        switch (node.kind) {
            case ts.SyntaxKind.SuperKeyword:
            case ts.SyntaxKind.NullKeyword:
            case ts.SyntaxKind.TrueKeyword:
            case ts.SyntaxKind.FalseKeyword:
            case ts.SyntaxKind.RegularExpressionLiteral:
            case ts.SyntaxKind.ArrayLiteralExpression:
            case ts.SyntaxKind.ObjectLiteralExpression:
            case ts.SyntaxKind.PropertyAccessExpression:
            case ts.SyntaxKind.ElementAccessExpression:
            case ts.SyntaxKind.CallExpression:
            case ts.SyntaxKind.NewExpression:
            case ts.SyntaxKind.TaggedTemplateExpression:
            case ts.SyntaxKind.AsExpression:
            case ts.SyntaxKind.TypeAssertionExpression:
            case ts.SyntaxKind.ParenthesizedExpression:
            case ts.SyntaxKind.FunctionExpression:
            case ts.SyntaxKind.ClassExpression:
            case ts.SyntaxKind.ArrowFunction:
            case ts.SyntaxKind.VoidExpression:
            case ts.SyntaxKind.DeleteExpression:
            case ts.SyntaxKind.TypeOfExpression:
            case ts.SyntaxKind.PrefixUnaryExpression:
            case ts.SyntaxKind.PostfixUnaryExpression:
            case ts.SyntaxKind.BinaryExpression:
            case ts.SyntaxKind.ConditionalExpression:
            case ts.SyntaxKind.SpreadElementExpression:
            case ts.SyntaxKind.TemplateExpression:
            case ts.SyntaxKind.NoSubstitutionTemplateLiteral:
            case ts.SyntaxKind.OmittedExpression:
            case ts.SyntaxKind.JsxElement:
            case ts.SyntaxKind.JsxSelfClosingElement:
            case ts.SyntaxKind.YieldExpression:
            case ts.SyntaxKind.AwaitExpression:
                return true;
            case ts.SyntaxKind.QualifiedName:
                while (node.parent.kind === ts.SyntaxKind.QualifiedName) {
                    node = node.parent;
                }
                return node.parent.kind === ts.SyntaxKind.TypeQuery;
            case ts.SyntaxKind.Identifier:
                if (node.parent.kind === ts.SyntaxKind.TypeQuery) {
                    return true;
                }
            // fall through
            case ts.SyntaxKind.NumericLiteral:
            case ts.SyntaxKind.StringLiteral:
            case ts.SyntaxKind.ThisKeyword:
                let parent = node.parent;
                switch (parent.kind) {
                    case ts.SyntaxKind.VariableDeclaration:
                    case ts.SyntaxKind.Parameter:
                    case ts.SyntaxKind.PropertyDeclaration:
                    case ts.SyntaxKind.PropertySignature:
                    case ts.SyntaxKind.EnumMember:
                    case ts.SyntaxKind.PropertyAssignment:
                    case ts.SyntaxKind.BindingElement:
                        return (<ts.VariableLikeDeclaration>parent).initializer === node;
                    case ts.SyntaxKind.ExpressionStatement:
                    case ts.SyntaxKind.IfStatement:
                    case ts.SyntaxKind.DoStatement:
                    case ts.SyntaxKind.WhileStatement:
                    case ts.SyntaxKind.ReturnStatement:
                    case ts.SyntaxKind.WithStatement:
                    case ts.SyntaxKind.SwitchStatement:
                    case ts.SyntaxKind.CaseClause:
                    case ts.SyntaxKind.ThrowStatement:
                    case ts.SyntaxKind.SwitchStatement:
                        return (<ts.ExpressionStatement>parent).expression === node;
                    case ts.SyntaxKind.ForStatement:
                        let forStatement = <ts.ForStatement>parent;
                        return (forStatement.initializer === node && forStatement.initializer.kind !== ts.SyntaxKind.VariableDeclarationList) ||
                            forStatement.condition === node ||
                            forStatement.incrementor === node;
                    case ts.SyntaxKind.ForInStatement:
                    case ts.SyntaxKind.ForOfStatement:
                        let forInStatement = <ts.ForInStatement | ts.ForOfStatement>parent;
                        return (forInStatement.initializer === node && forInStatement.initializer.kind !== ts.SyntaxKind.VariableDeclarationList) ||
                            forInStatement.expression === node;
                    case ts.SyntaxKind.TypeAssertionExpression:
                    case ts.SyntaxKind.AsExpression:
                        return node === (<ts.AssertionExpression>parent).expression;
                    case ts.SyntaxKind.TemplateSpan:
                        return node === (<ts.TemplateSpan>parent).expression;
                    case ts.SyntaxKind.ComputedPropertyName:
                        return node === (<ts.ComputedPropertyName>parent).expression;
                    case ts.SyntaxKind.Decorator:
                    case ts.SyntaxKind.JsxExpression:
                    case ts.SyntaxKind.JsxSpreadAttribute:
                        return true;
                    case ts.SyntaxKind.ExpressionWithTypeArguments:
                        return (<ts.ExpressionWithTypeArguments>parent).expression === node && isExpressionWithTypeArgumentsInClassExtendsClause(parent);
                    default:
                        if (isExpression(parent)) {
                            return true;
                        }
                }
        }
        return false;
    }