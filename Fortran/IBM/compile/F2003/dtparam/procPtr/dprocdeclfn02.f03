MODULE M
 TYPE,ABSTRACT :: Base(K1,Len1)
  private
  INTEGER,KIND :: K1
  INTEGER,LEN  :: Len1
  INTEGER(K1)  :: Num
  INTEGER(K1),private  :: Num2
 END TYPE

 TYPE,EXTENDS(Base) :: Base1
  CONTAINS
   PROCEDURE,PASS(OBJ) :: Get_Properties0 => sub2
   PROCEDURE,PASS      :: Get_Properties1 => sub2
   PROCEDURE,NOPASS    :: Get_Properties2 => sub2
 END TYPE

 ABSTRACT INTERFACE
  subroutine sub1(OBJ)
  import Base1
   CLASS(Base1(4,*)),INTENT(IN) :: OBJ
  end subroutine
 END INTERFACE

CONTAINS
  subroutine sub2(OBJ)
   CLASS(Base1(4,*)),INTENT(IN) :: OBJ
    print*,OBJ%Num2
  end subroutine

END MODULE

program dprocdeclfn02
use M

end program
