import * as ts from "typescript";
import { Location } from "../protocol/types";
import log from "../log";

/* Taken from typescript compiler because they are not exported. */

export function nodeContainsPoint(node: ts.Node, point: number): boolean {
    return node.getStart() <= point && node.getEnd() >= point;
}

export function isExportedNode(node: ts.Node): boolean {
    return (node.modifiers != null)
        && node.modifiers.filter(modifierNode => modifierNode.kind === ts.SyntaxKind.ExportKeyword).length > 0;
}

/* Ripped from palantir's tslint */
export function isScopeBoundary(node: ts.Node): boolean {
    return node.kind === ts.SyntaxKind.FunctionDeclaration
        || node.kind === ts.SyntaxKind.FunctionExpression
        || node.kind === ts.SyntaxKind.PropertyAssignment
        || node.kind === ts.SyntaxKind.ShorthandPropertyAssignment
        || node.kind === ts.SyntaxKind.MethodDeclaration
        || node.kind === ts.SyntaxKind.Constructor
        || node.kind === ts.SyntaxKind.ModuleDeclaration
        || node.kind === ts.SyntaxKind.ArrowFunction
        || node.kind === ts.SyntaxKind.ParenthesizedExpression
        || node.kind === ts.SyntaxKind.ClassDeclaration
        || node.kind === ts.SyntaxKind.ClassExpression
        || node.kind === ts.SyntaxKind.InterfaceDeclaration
        || node.kind === ts.SyntaxKind.GetAccessor
        || node.kind === ts.SyntaxKind.SetAccessor;
}

export function isBlockScopeBoundary(node: ts.Node): boolean {
    return isScopeBoundary(node)
        || node.kind === ts.SyntaxKind.DoStatement
        || node.kind === ts.SyntaxKind.WhileStatement
        || node.kind === ts.SyntaxKind.ForStatement
        || node.kind === ts.SyntaxKind.ForInStatement
        || node.kind === ts.SyntaxKind.ForOfStatement
        || node.kind === ts.SyntaxKind.WithStatement
        || node.kind === ts.SyntaxKind.SwitchStatement
        || (node.parent != null
            && (node.parent.kind === ts.SyntaxKind.TryStatement
                || node.parent.kind === ts.SyntaxKind.IfStatement)
           )
        || (node.kind === ts.SyntaxKind.Block && node.parent != null
            && (node.parent.kind === ts.SyntaxKind.Block
                || node.parent.kind === ts.SyntaxKind.SourceFile)
           );
}

export function isClassLike(node: ts.Node): node is ts.ClassLikeDeclaration {
    return node && (node.kind === ts.SyntaxKind.ClassDeclaration || node.kind === ts.SyntaxKind.ClassExpression);
}

export function isExpressionWithTypeArgumentsInClassExtendsClause(node: ts.Node): boolean {
    return node.kind === ts.SyntaxKind.ExpressionWithTypeArguments &&
        (<ts.HeritageClause>node.parent).token === ts.SyntaxKind.ExtendsKeyword &&
        (node.parent != null && node.parent.parent != null && isClassLike(node.parent.parent));
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
        case ts.SyntaxKind.SpreadAssignment:
        case ts.SyntaxKind.SpreadElement:
        case ts.SyntaxKind.TemplateExpression:
        case ts.SyntaxKind.NoSubstitutionTemplateLiteral:
        case ts.SyntaxKind.OmittedExpression:
        case ts.SyntaxKind.JsxElement:
        case ts.SyntaxKind.JsxSelfClosingElement:
        case ts.SyntaxKind.YieldExpression:
        case ts.SyntaxKind.AwaitExpression:
        return true;
        case ts.SyntaxKind.QualifiedName:
        while (node.parent!.kind === ts.SyntaxKind.QualifiedName) {
            node = node.parent!;
        }
        return node.parent!.kind === ts.SyntaxKind.TypeQuery;
        case ts.SyntaxKind.Identifier:
        if (node.parent!.kind === ts.SyntaxKind.TypeQuery) {
            return true;
        }
        // fall through
        case ts.SyntaxKind.NumericLiteral:
        case ts.SyntaxKind.StringLiteral:
        case ts.SyntaxKind.ThisKeyword:
        let parent = node.parent;
        switch (parent!.kind) {
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
            return (<ts.ExpressionStatement> parent).expression === node;
            case ts.SyntaxKind.ForStatement:
            let forStatement = <ts.ForStatement> parent!;
            return (forStatement.initializer === node && (forStatement.initializer && forStatement.initializer.kind) !== ts.SyntaxKind.VariableDeclarationList) ||
                forStatement.condition === node ||
                forStatement.incrementor === node;
            case ts.SyntaxKind.ForInStatement:
            case ts.SyntaxKind.ForOfStatement:
            let forInStatement = <ts.ForInStatement | ts.ForOfStatement> parent;
            return (forInStatement.initializer === node && forInStatement.initializer.kind !== ts.SyntaxKind.VariableDeclarationList) ||
                forInStatement.expression === node;
            case ts.SyntaxKind.TypeAssertionExpression:
            case ts.SyntaxKind.AsExpression:
            return node === (<ts.AssertionExpression> parent).expression;
            case ts.SyntaxKind.TemplateSpan:
            return node === (<ts.TemplateSpan> parent).expression;
            case ts.SyntaxKind.ComputedPropertyName:
            return node === (<ts.ComputedPropertyName> parent).expression;
            case ts.SyntaxKind.Decorator:
            case ts.SyntaxKind.JsxExpression:
            case ts.SyntaxKind.JsxSpreadAttribute:
            return true;
            case ts.SyntaxKind.ExpressionWithTypeArguments:
            return (<ts.ExpressionWithTypeArguments> parent).expression === node
                && isExpressionWithTypeArgumentsInClassExtendsClause(parent!);
            default:
            if (isExpression(parent!)) {
                return true;
            }
        }
    }
    return false;
}

/* Assumes that SourceFile retains the full text. Given a 0-indexed position (as received from `getStart()`),
   returns the correct `Location` in the file for use with the CodeEdit protocol. */
export function convertPositionToLocation(sourceFile: ts.SourceFile, position: number): Location {
    const lineAndCharacter = sourceFile.getLineAndCharacterOfPosition(position);

    return {
        line: lineAndCharacter.line + 1,
        offset: lineAndCharacter.character + 1
    };
}

export function getNodesContainingPoint(sourceFile: ts.SourceFile, point: number) {
    const results: ts.Node[] = [];

    function visitNode(node: ts.Node) {
        if (nodeContainsPoint(node, point)) {
            results.push(node);
            ts.forEachChild(node, visitNode);
        }
    }

    ts.forEachChild(sourceFile, visitNode);

    return results;
}

export function getMemberNames(node: ts.ClassDeclaration): string[] {
    const result: string[] = [];

    node.members.forEach(member => {
        if (member.kind === ts.SyntaxKind.MethodDeclaration) {
            result.push((member as ts.MethodDeclaration).name.getText());
        } else if (member.kind === ts.SyntaxKind.PropertyDeclaration) {
            result.push((member as ts.PropertyDeclaration).name.getText());
        }
    });

    return result;
}
