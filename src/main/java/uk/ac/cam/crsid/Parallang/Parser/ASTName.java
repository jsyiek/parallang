package uk.ac.cam.crsid.Parallang.Parser;

/* Generated By:JJTree: Do not edit this line. ASTName.java Version 4.3 */
/* JavaCCOptions:MULTI=true,NODE_USES_PARSER=false,VISITOR=false,TRACK_TOKENS=false,NODE_PREFIX=AST,NODE_EXTENDS=,NODE_FACTORY=,SUPPORT_CLASS_VISIBILITY_PUBLIC=true */
public
class ASTName extends SimpleNode {
  private String name;
  public void setName(String n) {
    name = n;
  }
  public String getName() {
    return name;
  }
  public ASTName(int id) {
    super(id);
  }

  public ASTName(ParallogParser p, int id) {
    super(p, id);
  }

  public String toString() {
    return "Name: " + name;
  }

}
/* JavaCC - OriginalChecksum=0600925e584e847c35ad34d2e29d8389 (do not edit this line) */
