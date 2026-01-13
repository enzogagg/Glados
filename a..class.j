.class public CladProgram
.super java/lang/Object

.method public <init>()V
   aload_0
   invokespecial java/lang/Object/<init>()V
   return
.end method

.method public static main([Ljava/lang/String;)V
  .limit stack 10
  .limit locals 10
  ldc 0
  istore 0
WhileStart_0:
  iload 0
  ldc 5
  if_icmplt cmpTrue_2
  ldc 0
  goto cmpEnd_3
cmpTrue_2:
  ldc 1
cmpEnd_3:
  ifeq WhileEnd_1
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload 0
  invokevirtual java/io/PrintStream/println(I)V
  iload 0
  ldc 1
  iadd
  istore 0
  goto WhileStart_0
WhileEnd_1:
  ldc 0
  pop
  return
.end method

