package org.sysml.ast;

import org.clafer.common.Check;

import java.io.IOException;

/**
 * SysML Property
 *
 * <property> ::= [<block-visibility>] [‘/’] [<property-type-keywords>] <name-string> <property-declaration>
 *
 * TODO: do the declaration
 */
public class SysmlProperty implements SysmlBlockDefElement {

    private final String name;

    private final SysmlPropertyType prop;

    private final SysmlBlockDefElement[] elements;

    private final SysmlAttribute[] annotations;
    private final String[] superTypes;

    private int multiplicity;



    public SysmlProperty(
            SysmlBlockVisibility blockVis,
            SysmlPropertyType propType,
            String name,
            SysmlBlockDefElement[] elements,
            SysmlAttribute[] annotations,
            String[] superTypes,
            int multiplicity
    ){
        this.name = Check.notNull(name);
        this.prop = propType;
        this.elements = elements;
        this.superTypes = superTypes;
        this.multiplicity = multiplicity;
        this.annotations = annotations;
    }

    public SysmlProperty(SysmlBlockVisibility blockVis, SysmlPropertyType propType, String name){
        this.name = Check.notNull(name);
        this.prop = propType;
        this.elements = new SysmlBlockDefElement[0];
        this.superTypes = new String[0];
        this.annotations = new SysmlAttribute[0];
        this.multiplicity = 1;
    }

    @Override
    public String getName() {
        return name;
    }

    public  SysmlPropertyType getPropertyType(){
        return prop;
    }

    public SysmlBlockDefElement[] getElements() {
        return elements;
    }

    public String[] getSupers(){
        return superTypes;
    }

    public int getMultiplicity(){
        return multiplicity;
    }

    public SysmlAttribute[] getAnnotations(){
        return annotations;
    }

    @Override
    public <A, B> B accept(SysmlExprVisitor<A, B> visitor, A a) throws IOException {
        return visitor.visit(this, a);
    }
}
