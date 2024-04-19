package org.sysml.ast;

/**
 * Sysml Property Keyword
 * <property-keyword> ::= ‘part’ | ‘reference’ | ‘value’ | <name-string>
 *
 *  TODO: accoutn for special cases
 */
public class SysmlPropertyType implements SysmlId {
    private final String name;
    public SysmlPropertyType(String name){
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }
}
