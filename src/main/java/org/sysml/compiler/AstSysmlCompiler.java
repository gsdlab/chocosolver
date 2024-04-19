package org.sysml.compiler;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.instance.InstanceClafer;
import org.sysml.ast.*;

import java.util.*;

public class AstSysmlCompiler {
    private Map<String, String> typeMap;

    public AstSysmlCompiler(){
        this.typeMap = new HashMap<>();
        this.typeMap.put("int", "Integer");
        this.typeMap.put("real", "Real");
        this.typeMap.put("double", "Real");
        this.typeMap.put("string", "String");
    }

    public SysmlPropertyDef[] compile(AstModel model, AstModel topLevelModel) {
        ArrayList<SysmlPropertyDef> propDefs = new ArrayList<SysmlPropertyDef>();

        for (AstAbstractClafer child: model.getAbstractRoot().getAbstractChildren()){
            String name =   SysmlCompilerUtils.getPropertyId(child.getName());

            ArrayList<SysmlAttribute> attrs = new ArrayList<SysmlAttribute>();
            for (AstClafer sub: child.getChildren()) {
                Object sref = sub.getRef();
                if (sref instanceof AstRef){
                    AstRef ref = (AstRef) sref;
                    Object tgt = typeMap.get(ref.getTargetType().toString());
                    String propName = SysmlCompilerUtils.getPropertyId(ref.getSourceType().getName());
                    attrs.add(new SysmlAttribute(propName, tgt));
                }
            }

            String[] superClafers = SysmlCompilerUtils.getSuperClafers(child);
            List hierarchy = Arrays.asList(superClafers);
            if (hierarchy.size() < 3){
                continue;
            }

            String[] superTypes = new String[0];
            if (hierarchy.contains("SysmlProperty")) {
                superTypes = Arrays.copyOfRange(superClafers, 0, hierarchy.indexOf("SysmlProperty")-1);
            }

            propDefs.add(new SysmlPropertyDef(
                    new SysmlBlockVisibility(SysmlVisibilityOption.PLUS),
                    new SysmlPropertyType("part"),
                    name,
                    new SysmlBlockDefElement[0],
                    attrs.toArray(new SysmlAttribute[attrs.size()]),
                    superTypes
            ));

        }

        return propDefs.toArray(new SysmlPropertyDef[propDefs.size()]);
    }
}
