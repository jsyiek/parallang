/* Generated By:JJTree&JavaCC: Do not edit this line. ParallogParserTokenManager.java */
package uk.ac.cam.crsid.Parallang.Parser;
import scala.Option;

/** Token Manager. */
public class ParallogParserTokenManager implements ParallogParserConstants
{

  /** Debug output. */
  public  java.io.PrintStream debugStream = System.out;
  /** Set debug output. */
  public  void setDebugStream(java.io.PrintStream ds) { debugStream = ds; }
private final int jjStopStringLiteralDfa_0(int pos, long active0, long active1)
{
   switch (pos)
   {
      case 0:
         if ((active0 & 0x47fffffffeL) != 0L)
         {
            jjmatchedKind = 39;
            return 1;
         }
         if ((active1 & 0x10L) != 0L)
            return 4;
         return -1;
      case 1:
         if ((active0 & 0x10002L) != 0L)
            return 1;
         if ((active0 & 0x47fffefffcL) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 1;
            return 1;
         }
         return -1;
      case 2:
         if ((active0 & 0x406004868cL) != 0L)
            return 1;
         if ((active0 & 0x79ffa7970L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 2;
            return 1;
         }
         return -1;
      case 3:
         if ((active0 & 0x626120L) != 0L)
            return 1;
         if ((active0 & 0x79f981850L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 3;
            return 1;
         }
         return -1;
      case 4:
         if ((active0 & 0x80010L) != 0L)
            return 1;
         if ((active0 & 0x79f901840L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 4;
            return 1;
         }
         return -1;
      case 5:
         if ((active0 & 0x79e800800L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 5;
            return 1;
         }
         if ((active0 & 0x1101040L) != 0L)
            return 1;
         return -1;
      case 6:
         if ((active0 & 0x61e000000L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 6;
            return 1;
         }
         if ((active0 & 0x180800800L) != 0L)
            return 1;
         return -1;
      case 7:
         if ((active0 & 0x60e000000L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 7;
            return 1;
         }
         if ((active0 & 0x10000000L) != 0L)
            return 1;
         return -1;
      case 8:
         if ((active0 & 0x600000000L) != 0L)
         {
            if (jjmatchedPos != 8)
            {
               jjmatchedKind = 39;
               jjmatchedPos = 8;
            }
            return 1;
         }
         if ((active0 & 0xe000000L) != 0L)
            return 1;
         return -1;
      case 9:
         if ((active0 & 0xc000000L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 9;
            return 1;
         }
         if ((active0 & 0x600000000L) != 0L)
            return 1;
         return -1;
      case 10:
         if ((active0 & 0xc000000L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 10;
            return 1;
         }
         return -1;
      case 11:
         if ((active0 & 0xc000000L) != 0L)
         {
            jjmatchedKind = 39;
            jjmatchedPos = 11;
            return 1;
         }
         return -1;
      default :
         return -1;
   }
}
private final int jjStartNfa_0(int pos, long active0, long active1)
{
   return jjMoveNfa_0(jjStopStringLiteralDfa_0(pos, active0, active1), pos + 1);
}
private int jjStopAtPos(int pos, int kind)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   return pos + 1;
}
private int jjMoveStringLiteralDfa0_0()
{
   switch(curChar)
   {
      case 33:
         jjmatchedKind = 35;
         return jjMoveStringLiteralDfa1_0(0x0L, 0x1L);
      case 38:
         return jjMoveStringLiteralDfa1_0(0x2000000000L, 0x0L);
      case 40:
         return jjStopAtPos(0, 50);
      case 41:
         return jjStopAtPos(0, 51);
      case 42:
         return jjStopAtPos(0, 67);
      case 43:
         return jjStopAtPos(0, 65);
      case 44:
         return jjStopAtPos(0, 56);
      case 45:
         jjmatchedKind = 66;
         return jjMoveStringLiteralDfa1_0(0x10000000000000L, 0x0L);
      case 46:
         return jjStopAtPos(0, 69);
      case 47:
         return jjStartNfaWithStates_0(0, 68, 4);
      case 58:
         return jjStopAtPos(0, 55);
      case 59:
         return jjStopAtPos(0, 49);
      case 60:
         jjmatchedKind = 60;
         return jjMoveStringLiteralDfa1_0(0x4400000000000000L, 0x0L);
      case 61:
         jjmatchedKind = 57;
         return jjMoveStringLiteralDfa1_0(0x8000000000000000L, 0x0L);
      case 62:
         jjmatchedKind = 59;
         return jjMoveStringLiteralDfa1_0(0x2000000000000000L, 0x0L);
      case 78:
         return jjMoveStringLiteralDfa1_0(0x380000000L, 0x0L);
      case 91:
         return jjStopAtPos(0, 47);
      case 93:
         return jjStopAtPos(0, 48);
      case 97:
         return jjMoveStringLiteralDfa1_0(0x10L, 0x0L);
      case 98:
         return jjMoveStringLiteralDfa1_0(0xe000000L, 0x0L);
      case 101:
         return jjMoveStringLiteralDfa1_0(0x10020000L, 0x0L);
      case 102:
         return jjMoveStringLiteralDfa1_0(0x40002L, 0x0L);
      case 103:
         return jjMoveStringLiteralDfa1_0(0x400L, 0x0L);
      case 105:
         return jjMoveStringLiteralDfa1_0(0x4400010008L, 0x0L);
      case 108:
         return jjMoveStringLiteralDfa1_0(0x220L, 0x0L);
      case 109:
         return jjMoveStringLiteralDfa1_0(0x60008000L, 0x0L);
      case 110:
         return jjMoveStringLiteralDfa1_0(0x2080L, 0x0L);
      case 111:
         return jjMoveStringLiteralDfa1_0(0x1000L, 0x0L);
      case 112:
         return jjMoveStringLiteralDfa1_0(0x800L, 0x0L);
      case 114:
         return jjMoveStringLiteralDfa1_0(0x500000L, 0x0L);
      case 115:
         return jjMoveStringLiteralDfa1_0(0x204040L, 0x0L);
      case 116:
         return jjMoveStringLiteralDfa1_0(0x800000L, 0x0L);
      case 117:
         return jjMoveStringLiteralDfa1_0(0x100L, 0x0L);
      case 118:
         return jjMoveStringLiteralDfa1_0(0x4L, 0x0L);
      case 119:
         return jjMoveStringLiteralDfa1_0(0x1080000L, 0x0L);
      case 123:
         return jjStopAtPos(0, 53);
      case 124:
         return jjMoveStringLiteralDfa1_0(0x1000000000L, 0x0L);
      case 125:
         return jjStopAtPos(0, 54);
      default :
         return jjMoveNfa_0(0, 0);
   }
}
private int jjMoveStringLiteralDfa1_0(long active0, long active1)
{
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(0, active0, active1);
      return 1;
   }
   switch(curChar)
   {
      case 38:
         if ((active0 & 0x2000000000L) != 0L)
            return jjStopAtPos(1, 37);
         break;
      case 45:
         if ((active0 & 0x400000000000000L) != 0L)
            return jjStopAtPos(1, 58);
         break;
      case 61:
         if ((active0 & 0x2000000000000000L) != 0L)
            return jjStopAtPos(1, 61);
         else if ((active0 & 0x4000000000000000L) != 0L)
            return jjStopAtPos(1, 62);
         else if ((active0 & 0x8000000000000000L) != 0L)
            return jjStopAtPos(1, 63);
         else if ((active1 & 0x1L) != 0L)
            return jjStopAtPos(1, 64);
         break;
      case 62:
         if ((active0 & 0x10000000000000L) != 0L)
            return jjStopAtPos(1, 52);
         break;
      case 97:
         return jjMoveStringLiteralDfa2_0(active0, 0x8004L, active1, 0L);
      case 101:
         return jjMoveStringLiteralDfa2_0(active0, 0x700680L, active1, 0L);
      case 102:
         if ((active0 & 0x10000L) != 0L)
            return jjStartNfaWithStates_0(1, 16, 1);
         break;
      case 104:
         return jjMoveStringLiteralDfa2_0(active0, 0x80000L, active1, 0L);
      case 105:
         return jjMoveStringLiteralDfa2_0(active0, 0x20L, active1, 0L);
      case 108:
         return jjMoveStringLiteralDfa2_0(active0, 0x20000L, active1, 0L);
      case 110:
         if ((active0 & 0x2L) != 0L)
            return jjStartNfaWithStates_0(1, 1, 1);
         return jjMoveStringLiteralDfa2_0(active0, 0x4000000108L, active1, 0L);
      case 111:
         return jjMoveStringLiteralDfa2_0(active0, 0x1046000L, active1, 0L);
      case 112:
         return jjMoveStringLiteralDfa2_0(active0, 0x1000L, active1, 0L);
      case 114:
         return jjMoveStringLiteralDfa2_0(active0, 0xe800810L, active1, 0L);
      case 115:
         return jjMoveStringLiteralDfa2_0(active0, 0x400000000L, active1, 0L);
      case 116:
         return jjMoveStringLiteralDfa2_0(active0, 0x40L, active1, 0L);
      case 117:
         return jjMoveStringLiteralDfa2_0(active0, 0x380000000L, active1, 0L);
      case 120:
         return jjMoveStringLiteralDfa2_0(active0, 0x10000000L, active1, 0L);
      case 121:
         return jjMoveStringLiteralDfa2_0(active0, 0x60000000L, active1, 0L);
      case 124:
         if ((active0 & 0x1000000000L) != 0L)
            return jjStopAtPos(1, 36);
         break;
      default :
         break;
   }
   return jjStartNfa_0(0, active0, active1);
}
private int jjMoveStringLiteralDfa2_0(long old0, long active0, long old1, long active1)
{
   if (((active0 &= old0) | (active1 &= old1)) == 0L)
      return jjStartNfa_0(0, old0, old1);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(1, active0, 0L);
      return 2;
   }
   switch(curChar)
   {
      case 77:
         return jjMoveStringLiteralDfa3_0(active0, 0x400000000L);
      case 88:
         if ((active0 & 0x20000000L) != 0L)
            return jjStartNfaWithStates_0(2, 29, 1);
         break;
      case 89:
         if ((active0 & 0x40000000L) != 0L)
            return jjStartNfaWithStates_0(2, 30, 1);
         break;
      case 99:
         return jjMoveStringLiteralDfa3_0(active0, 0x400000L);
      case 102:
         if ((active0 & 0x4000000000L) != 0L)
            return jjStartNfaWithStates_0(2, 38, 1);
         break;
      case 105:
         return jjMoveStringLiteralDfa3_0(active0, 0x80900L);
      case 109:
         return jjMoveStringLiteralDfa3_0(active0, 0x380004000L);
      case 110:
         if ((active0 & 0x200L) != 0L)
            return jjStartNfaWithStates_0(2, 9, 1);
         return jjMoveStringLiteralDfa3_0(active0, 0x202000L);
      case 111:
         return jjMoveStringLiteralDfa3_0(active0, 0xe000000L);
      case 112:
         if ((active0 & 0x8000L) != 0L)
            return jjStartNfaWithStates_0(2, 15, 1);
         break;
      case 114:
         if ((active0 & 0x4L) != 0L)
            return jjStartNfaWithStates_0(2, 2, 1);
         else if ((active0 & 0x40000L) != 0L)
            return jjStartNfaWithStates_0(2, 18, 1);
         return jjMoveStringLiteralDfa3_0(active0, 0x1000050L);
      case 115:
         return jjMoveStringLiteralDfa3_0(active0, 0x20020L);
      case 116:
         if ((active0 & 0x8L) != 0L)
            return jjStartNfaWithStates_0(2, 3, 1);
         else if ((active0 & 0x400L) != 0L)
            return jjStartNfaWithStates_0(2, 10, 1);
         return jjMoveStringLiteralDfa3_0(active0, 0x10101000L);
      case 119:
         if ((active0 & 0x80L) != 0L)
            return jjStartNfaWithStates_0(2, 7, 1);
         break;
      case 121:
         return jjMoveStringLiteralDfa3_0(active0, 0x800000L);
      default :
         break;
   }
   return jjStartNfa_0(1, active0, 0L);
}
private int jjMoveStringLiteralDfa3_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(1, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(2, active0, 0L);
      return 3;
   }
   switch(curChar)
   {
      case 67:
         return jjMoveStringLiteralDfa4_0(active0, 0x100000000L);
      case 82:
         return jjMoveStringLiteralDfa4_0(active0, 0x80000000L);
      case 87:
         return jjMoveStringLiteralDfa4_0(active0, 0x200000000L);
      case 97:
         return jjMoveStringLiteralDfa4_0(active0, 0x40e000010L);
      case 100:
         if ((active0 & 0x200000L) != 0L)
            return jjStartNfaWithStates_0(3, 21, 1);
         break;
      case 101:
         if ((active0 & 0x2000L) != 0L)
            return jjStartNfaWithStates_0(3, 13, 1);
         else if ((active0 & 0x4000L) != 0L)
            return jjStartNfaWithStates_0(3, 14, 1);
         else if ((active0 & 0x20000L) != 0L)
            return jjStartNfaWithStates_0(3, 17, 1);
         return jjMoveStringLiteralDfa4_0(active0, 0x10000000L);
      case 105:
         return jjMoveStringLiteralDfa4_0(active0, 0x1000L);
      case 107:
         return jjMoveStringLiteralDfa4_0(active0, 0x1000000L);
      case 108:
         return jjMoveStringLiteralDfa4_0(active0, 0x80000L);
      case 110:
         return jjMoveStringLiteralDfa4_0(active0, 0x800L);
      case 114:
         return jjMoveStringLiteralDfa4_0(active0, 0x800000L);
      case 116:
         if ((active0 & 0x20L) != 0L)
            return jjStartNfaWithStates_0(3, 5, 1);
         else if ((active0 & 0x100L) != 0L)
            return jjStartNfaWithStates_0(3, 8, 1);
         break;
      case 117:
         return jjMoveStringLiteralDfa4_0(active0, 0x100040L);
      case 118:
         if ((active0 & 0x400000L) != 0L)
            return jjStartNfaWithStates_0(3, 22, 1);
         break;
      default :
         break;
   }
   return jjStartNfa_0(2, active0, 0L);
}
private int jjMoveStringLiteralDfa4_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(2, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(3, active0, 0L);
      return 4;
   }
   switch(curChar)
   {
      case 99:
         return jjMoveStringLiteralDfa5_0(active0, 0x40L);
      case 100:
         return jjMoveStringLiteralDfa5_0(active0, 0xe000000L);
      case 101:
         if ((active0 & 0x80000L) != 0L)
            return jjStartNfaWithStates_0(4, 19, 1);
         return jjMoveStringLiteralDfa5_0(active0, 0x1800000L);
      case 105:
         return jjMoveStringLiteralDfa5_0(active0, 0x400000000L);
      case 111:
         return jjMoveStringLiteralDfa5_0(active0, 0x380001000L);
      case 114:
         return jjMoveStringLiteralDfa5_0(active0, 0x10100000L);
      case 116:
         return jjMoveStringLiteralDfa5_0(active0, 0x800L);
      case 121:
         if ((active0 & 0x10L) != 0L)
            return jjStartNfaWithStates_0(4, 4, 1);
         break;
      default :
         break;
   }
   return jjStartNfa_0(3, active0, 0L);
}
private int jjMoveStringLiteralDfa5_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(3, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(4, active0, 0L);
      return 5;
   }
   switch(curChar)
   {
      case 99:
         return jjMoveStringLiteralDfa6_0(active0, 0xe800000L);
      case 108:
         return jjMoveStringLiteralDfa6_0(active0, 0x100000800L);
      case 110:
         if ((active0 & 0x1000L) != 0L)
            return jjStartNfaWithStates_0(5, 12, 1);
         else if ((active0 & 0x100000L) != 0L)
            return jjStartNfaWithStates_0(5, 20, 1);
         return jjMoveStringLiteralDfa6_0(active0, 0x410000000L);
      case 114:
         if ((active0 & 0x1000000L) != 0L)
            return jjStartNfaWithStates_0(5, 24, 1);
         return jjMoveStringLiteralDfa6_0(active0, 0x200000000L);
      case 116:
         if ((active0 & 0x40L) != 0L)
            return jjStartNfaWithStates_0(5, 6, 1);
         break;
      case 119:
         return jjMoveStringLiteralDfa6_0(active0, 0x80000000L);
      default :
         break;
   }
   return jjStartNfa_0(4, active0, 0L);
}
private int jjMoveStringLiteralDfa6_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(4, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(5, active0, 0L);
      return 6;
   }
   switch(curChar)
   {
      case 67:
         return jjMoveStringLiteralDfa7_0(active0, 0x400000000L);
      case 97:
         return jjMoveStringLiteralDfa7_0(active0, 0x1e000000L);
      case 107:
         return jjMoveStringLiteralDfa7_0(active0, 0x200000000L);
      case 110:
         if ((active0 & 0x800L) != 0L)
            return jjStartNfaWithStates_0(6, 11, 1);
         break;
      case 115:
         if ((active0 & 0x80000000L) != 0L)
            return jjStartNfaWithStates_0(6, 31, 1);
         else if ((active0 & 0x100000000L) != 0L)
            return jjStartNfaWithStates_0(6, 32, 1);
         break;
      case 118:
         if ((active0 & 0x800000L) != 0L)
            return jjStartNfaWithStates_0(6, 23, 1);
         break;
      default :
         break;
   }
   return jjStartNfa_0(5, active0, 0L);
}
private int jjMoveStringLiteralDfa7_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(5, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(6, active0, 0L);
      return 7;
   }
   switch(curChar)
   {
      case 101:
         return jjMoveStringLiteralDfa8_0(active0, 0x200000000L);
      case 108:
         if ((active0 & 0x10000000L) != 0L)
            return jjStartNfaWithStates_0(7, 28, 1);
         break;
      case 111:
         return jjMoveStringLiteralDfa8_0(active0, 0x400000000L);
      case 115:
         return jjMoveStringLiteralDfa8_0(active0, 0xe000000L);
      default :
         break;
   }
   return jjStartNfa_0(6, active0, 0L);
}
private int jjMoveStringLiteralDfa8_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(6, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(7, active0, 0L);
      return 8;
   }
   switch(curChar)
   {
      case 114:
         return jjMoveStringLiteralDfa9_0(active0, 0x600000000L);
      case 116:
         if ((active0 & 0x2000000L) != 0L)
         {
            jjmatchedKind = 25;
            jjmatchedPos = 8;
         }
         return jjMoveStringLiteralDfa9_0(active0, 0xc000000L);
      default :
         break;
   }
   return jjStartNfa_0(7, active0, 0L);
}
private int jjMoveStringLiteralDfa9_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(7, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(8, active0, 0L);
      return 9;
   }
   switch(curChar)
   {
      case 95:
         return jjMoveStringLiteralDfa10_0(active0, 0xc000000L);
      case 101:
         if ((active0 & 0x400000000L) != 0L)
            return jjStartNfaWithStates_0(9, 34, 1);
         break;
      case 115:
         if ((active0 & 0x200000000L) != 0L)
            return jjStartNfaWithStates_0(9, 33, 1);
         break;
      default :
         break;
   }
   return jjStartNfa_0(8, active0, 0L);
}
private int jjMoveStringLiteralDfa10_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(8, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(9, active0, 0L);
      return 10;
   }
   switch(curChar)
   {
      case 99:
         return jjMoveStringLiteralDfa11_0(active0, 0x8000000L);
      case 114:
         return jjMoveStringLiteralDfa11_0(active0, 0x4000000L);
      default :
         break;
   }
   return jjStartNfa_0(9, active0, 0L);
}
private int jjMoveStringLiteralDfa11_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(9, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(10, active0, 0L);
      return 11;
   }
   switch(curChar)
   {
      case 111:
         return jjMoveStringLiteralDfa12_0(active0, 0xc000000L);
      default :
         break;
   }
   return jjStartNfa_0(10, active0, 0L);
}
private int jjMoveStringLiteralDfa12_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(10, old0, 0L);
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(11, active0, 0L);
      return 12;
   }
   switch(curChar)
   {
      case 108:
         if ((active0 & 0x8000000L) != 0L)
            return jjStartNfaWithStates_0(12, 27, 1);
         break;
      case 119:
         if ((active0 & 0x4000000L) != 0L)
            return jjStartNfaWithStates_0(12, 26, 1);
         break;
      default :
         break;
   }
   return jjStartNfa_0(11, active0, 0L);
}
private int jjStartNfaWithStates_0(int pos, int kind, int state)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) { return pos + 1; }
   return jjMoveNfa_0(state, pos + 1);
}
static final long[] jjbitVec0 = {
   0x0L, 0x0L, 0xffffffffffffffffL, 0xffffffffffffffffL
};
private int jjMoveNfa_0(int startState, int curPos)
{
   int startsAt = 0;
   jjnewStateCnt = 15;
   int i = 1;
   jjstateSet[0] = startState;
   int kind = 0x7fffffff;
   for (;;)
   {
      if (++jjround == 0x7fffffff)
         ReInitRounds();
      if (curChar < 64)
      {
         long l = 1L << curChar;
         do
         {
            switch(jjstateSet[--i])
            {
               case 0:
                  if ((0x3ff000000000000L & l) != 0L)
                  {
                     if (kind > 40)
                        kind = 40;
                     jjCheckNAdd(2);
                  }
                  else if (curChar == 47)
                     jjAddStates(0, 1);
                  break;
               case 4:
                  if (curChar == 42)
                     jjCheckNAddTwoStates(10, 11);
                  else if (curChar == 47)
                     jjCheckNAddStates(2, 4);
                  break;
               case 1:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 39)
                     kind = 39;
                  jjstateSet[jjnewStateCnt++] = 1;
                  break;
               case 2:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 40)
                     kind = 40;
                  jjCheckNAdd(2);
                  break;
               case 3:
                  if (curChar == 47)
                     jjAddStates(0, 1);
                  break;
               case 5:
                  if ((0xffffffffffffdbffL & l) != 0L)
                     jjCheckNAddStates(2, 4);
                  break;
               case 6:
                  if ((0x2400L & l) != 0L && kind > 45)
                     kind = 45;
                  break;
               case 7:
                  if (curChar == 10 && kind > 45)
                     kind = 45;
                  break;
               case 8:
                  if (curChar == 13)
                     jjstateSet[jjnewStateCnt++] = 7;
                  break;
               case 9:
                  if (curChar == 42)
                     jjCheckNAddTwoStates(10, 11);
                  break;
               case 10:
                  if ((0xfffffbffffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(10, 11);
                  break;
               case 11:
                  if (curChar == 42)
                     jjAddStates(5, 6);
                  break;
               case 12:
                  if ((0xffff7fffffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(13, 11);
                  break;
               case 13:
                  if ((0xfffffbffffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(13, 11);
                  break;
               case 14:
                  if (curChar == 47 && kind > 46)
                     kind = 46;
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else if (curChar < 128)
      {
         long l = 1L << (curChar & 077);
         do
         {
            switch(jjstateSet[--i])
            {
               case 0:
               case 1:
                  if ((0x7fffffe87fffffeL & l) == 0L)
                     break;
                  if (kind > 39)
                     kind = 39;
                  jjCheckNAdd(1);
                  break;
               case 5:
                  jjAddStates(2, 4);
                  break;
               case 10:
                  jjCheckNAddTwoStates(10, 11);
                  break;
               case 12:
               case 13:
                  jjCheckNAddTwoStates(13, 11);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else
      {
         int i2 = (curChar & 0xff) >> 6;
         long l2 = 1L << (curChar & 077);
         do
         {
            switch(jjstateSet[--i])
            {
               case 5:
                  if ((jjbitVec0[i2] & l2) != 0L)
                     jjAddStates(2, 4);
                  break;
               case 10:
                  if ((jjbitVec0[i2] & l2) != 0L)
                     jjCheckNAddTwoStates(10, 11);
                  break;
               case 12:
               case 13:
                  if ((jjbitVec0[i2] & l2) != 0L)
                     jjCheckNAddTwoStates(13, 11);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      if (kind != 0x7fffffff)
      {
         jjmatchedKind = kind;
         jjmatchedPos = curPos;
         kind = 0x7fffffff;
      }
      ++curPos;
      if ((i = jjnewStateCnt) == (startsAt = 15 - (jjnewStateCnt = startsAt)))
         return curPos;
      try { curChar = input_stream.readChar(); }
      catch(java.io.IOException e) { return curPos; }
   }
}
static final int[] jjnextStates = {
   4, 9, 5, 6, 8, 12, 14, 
};

/** Token literal values. */
public static final String[] jjstrLiteralImages = {
"", "\146\156", "\166\141\162", "\151\156\164", "\141\162\162\141\171", 
"\154\151\163\164", "\163\164\162\165\143\164", "\156\145\167", "\165\156\151\164", 
"\154\145\156", "\147\145\164", "\160\162\151\156\164\154\156", "\157\160\164\151\157\156", 
"\156\157\156\145", "\163\157\155\145", "\155\141\160", "\151\146", "\145\154\163\145", 
"\146\157\162", "\167\150\151\154\145", "\162\145\164\165\162\156", "\163\145\156\144", 
"\162\145\143\166", "\164\162\171\162\145\143\166", "\167\157\162\153\145\162", 
"\142\162\157\141\144\143\141\163\164", "\142\162\157\141\144\143\141\163\164\137\162\157\167", 
"\142\162\157\141\144\143\141\163\164\137\143\157\154", "\145\170\164\145\162\156\141\154", "\155\171\130", "\155\171\131", 
"\116\165\155\122\157\167\163", "\116\165\155\103\157\154\163", "\116\165\155\127\157\162\153\145\162\163", 
"\151\163\115\141\151\156\103\157\162\145", "\41", "\174\174", "\46\46", "\151\156\146", null, null, null, null, null, 
null, null, null, "\133", "\135", "\73", "\50", "\51", "\55\76", "\173", "\175", 
"\72", "\54", "\75", "\74\55", "\76", "\74", "\76\75", "\74\75", "\75\75", "\41\75", 
"\53", "\55", "\52", "\57", "\56", };

/** Lexer state names. */
public static final String[] lexStateNames = {
   "DEFAULT",
};
static final long[] jjtoToken = {
   0xffff81ffffffffffL, 0x3fL, 
};
static final long[] jjtoSkip = {
   0x7e0000000000L, 0x0L, 
};
protected SimpleCharStream input_stream;
private final int[] jjrounds = new int[15];
private final int[] jjstateSet = new int[30];
protected char curChar;
/** Constructor. */
public ParallogParserTokenManager(SimpleCharStream stream){
   if (SimpleCharStream.staticFlag)
      throw new Error("ERROR: Cannot use a static CharStream class with a non-static lexical analyzer.");
   input_stream = stream;
}

/** Constructor. */
public ParallogParserTokenManager(SimpleCharStream stream, int lexState){
   this(stream);
   SwitchTo(lexState);
}

/** Reinitialise parser. */
public void ReInit(SimpleCharStream stream)
{
   jjmatchedPos = jjnewStateCnt = 0;
   curLexState = defaultLexState;
   input_stream = stream;
   ReInitRounds();
}
private void ReInitRounds()
{
   int i;
   jjround = 0x80000001;
   for (i = 15; i-- > 0;)
      jjrounds[i] = 0x80000000;
}

/** Reinitialise parser. */
public void ReInit(SimpleCharStream stream, int lexState)
{
   ReInit(stream);
   SwitchTo(lexState);
}

/** Switch to specified lex state. */
public void SwitchTo(int lexState)
{
   if (lexState >= 1 || lexState < 0)
      throw new TokenMgrError("Error: Ignoring invalid lexical state : " + lexState + ". State unchanged.", TokenMgrError.INVALID_LEXICAL_STATE);
   else
      curLexState = lexState;
}

protected Token jjFillToken()
{
   final Token t;
   final String curTokenImage;
   final int beginLine;
   final int endLine;
   final int beginColumn;
   final int endColumn;
   String im = jjstrLiteralImages[jjmatchedKind];
   curTokenImage = (im == null) ? input_stream.GetImage() : im;
   beginLine = input_stream.getBeginLine();
   beginColumn = input_stream.getBeginColumn();
   endLine = input_stream.getEndLine();
   endColumn = input_stream.getEndColumn();
   t = Token.newToken(jjmatchedKind, curTokenImage);

   t.beginLine = beginLine;
   t.endLine = endLine;
   t.beginColumn = beginColumn;
   t.endColumn = endColumn;

   return t;
}

int curLexState = 0;
int defaultLexState = 0;
int jjnewStateCnt;
int jjround;
int jjmatchedPos;
int jjmatchedKind;

/** Get the next Token. */
public Token getNextToken() 
{
  Token matchedToken;
  int curPos = 0;

  EOFLoop :
  for (;;)
  {
   try
   {
      curChar = input_stream.BeginToken();
   }
   catch(java.io.IOException e)
   {
      jjmatchedKind = 0;
      matchedToken = jjFillToken();
      return matchedToken;
   }

   try { input_stream.backup(0);
      while (curChar <= 32 && (0x100002600L & (1L << curChar)) != 0L)
         curChar = input_stream.BeginToken();
   }
   catch (java.io.IOException e1) { continue EOFLoop; }
   jjmatchedKind = 0x7fffffff;
   jjmatchedPos = 0;
   curPos = jjMoveStringLiteralDfa0_0();
   if (jjmatchedKind != 0x7fffffff)
   {
      if (jjmatchedPos + 1 < curPos)
         input_stream.backup(curPos - jjmatchedPos - 1);
      if ((jjtoToken[jjmatchedKind >> 6] & (1L << (jjmatchedKind & 077))) != 0L)
      {
         matchedToken = jjFillToken();
         return matchedToken;
      }
      else
      {
         continue EOFLoop;
      }
   }
   int error_line = input_stream.getEndLine();
   int error_column = input_stream.getEndColumn();
   String error_after = null;
   boolean EOFSeen = false;
   try { input_stream.readChar(); input_stream.backup(1); }
   catch (java.io.IOException e1) {
      EOFSeen = true;
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
      if (curChar == '\n' || curChar == '\r') {
         error_line++;
         error_column = 0;
      }
      else
         error_column++;
   }
   if (!EOFSeen) {
      input_stream.backup(1);
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
   }
   throw new TokenMgrError(EOFSeen, curLexState, error_line, error_column, error_after, curChar, TokenMgrError.LEXICAL_ERROR);
  }
}

private void jjCheckNAdd(int state)
{
   if (jjrounds[state] != jjround)
   {
      jjstateSet[jjnewStateCnt++] = state;
      jjrounds[state] = jjround;
   }
}
private void jjAddStates(int start, int end)
{
   do {
      jjstateSet[jjnewStateCnt++] = jjnextStates[start];
   } while (start++ != end);
}
private void jjCheckNAddTwoStates(int state1, int state2)
{
   jjCheckNAdd(state1);
   jjCheckNAdd(state2);
}

private void jjCheckNAddStates(int start, int end)
{
   do {
      jjCheckNAdd(jjnextStates[start]);
   } while (start++ != end);
}

}