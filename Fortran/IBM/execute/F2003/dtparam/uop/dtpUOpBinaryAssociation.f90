!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpBinaryAssociation
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct association in expressions with repeated binary operations (left-to-right for all but **)
!*
!*  REFERENCE                  : Feature Number 361989
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Define several types with type-bound procedures and generic bindings for
!*  binary operators and verify correct associativity (left-to-right for all but **).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpBinaryAssociationmod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
   contains
     generic :: operator(+) => binaryPlusK
     procedure, pass :: binaryPlusK
     generic :: operator(-) => binaryMinusK
     procedure, pass :: binaryMinusK
     generic :: operator(*) => binaryStarK
     procedure, pass :: binaryStarK
     generic :: operator(/) => binarySlashK
     procedure, pass :: binarySlashK
     generic :: operator(**) => binaryPowerK
     procedure, pass :: binaryPowerK
  end type dk

  type dl (l)
     integer, len  :: l
     character(1)  :: cvar(l)
   contains
     generic :: operator(+) => binaryPlusL
     procedure, pass :: binaryPlusL
     generic :: operator(-) => binaryMinusL
     procedure, pass :: binaryMinusL
     generic :: operator(*) => binaryStarL
     procedure, pass :: binaryStarL
     generic :: operator(/) => binarySlashL
     procedure, pass :: binarySlashL
     generic :: operator(**) => binaryPowerL
     procedure, pass :: binaryPowerL
  end type dl

  type d2k (k,k2)
     integer, kind :: k, k2
     integer(k)    :: ivar
     integer(k2)   :: ivar2
   contains
     generic :: operator(+) => binaryPlus2K
     procedure, pass :: binaryPlus2K
     generic :: operator(-) => binaryMinus2K
     procedure, pass :: binaryMinus2K
     generic :: operator(*) => binaryStar2K
     procedure, pass :: binaryStar2K
     generic :: operator(/) => binarySlash2K
     procedure, pass :: binarySlash2K
     generic :: operator(**) => binaryPower2K
     procedure, pass :: binaryPower2K
  end type d2k

  type d2l (k,l)
     integer, kind :: k
     integer, len  :: l
     character(1)  :: cvar(l)
     integer(k)    :: ivar
   contains
     generic :: operator(+) => binaryPlus2L
     procedure, pass :: binaryPlus2L
     generic :: operator(-) => binaryMinus2L
     procedure, pass :: binaryMinus2L
     generic :: operator(*) => binaryStar2L
     procedure, pass :: binaryStar2L
     generic :: operator(/) => binarySlash2L
     procedure, pass :: binarySlash2L
     generic :: operator(**) => binaryPower2L
     procedure, pass :: binaryPower2L
  end type d2l

contains

  type(dk(4)) function binaryPlusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPlusK = dk(4)(this%ivar + that%ivar)
    print *, "binaryPlusK:", this%ivar, that%ivar, "->", binaryPlusK%ivar
  end function binaryPlusK

  type(dk(4)) function binaryMinusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryMinusK = dk(4)(this%ivar - that%ivar)
    print *, "binaryMinusK:", this%ivar, that%ivar, "->", binaryMinusK%ivar
  end function binaryMinusK

  type(dk(4)) function binaryStarK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryStarK = dk(4)(this%ivar * that%ivar)
    print *, "binaryStarK:", this%ivar, that%ivar, "->", binaryStarK%ivar
  end function binaryStarK

  type(dk(4)) function binarySlashK(this,that)
    class(dk(4)), intent(in) :: this, that
    binarySlashK = dk(4)(this%ivar / that%ivar)
    print *, "binarySlashK:", this%ivar, that%ivar, "->", binarySlashK%ivar
  end function binarySlashK

  type(dk(4)) function binaryPowerK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPowerK = dk(4)(this%ivar ** that%ivar)
    print *, "binaryPowerK:", this%ivar, that%ivar, "->", binaryPowerK%ivar
  end function binaryPowerK


  function binaryPlusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l+1)) :: binaryPlusL
    binaryPlusL = dl(this%l+1)([achar(abs(mod(iachar(this%cvar) + iachar(that%cvar), 96))+32), 'l'])
    print *, "binaryPlusL:", this%l, this%cvar, that%l, that%cvar, "->", binaryPlusL%l, binaryPlusL%cvar
  end function binaryPlusL

  function binaryMinusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l-1)) :: binaryMinusL
    binaryMinusL = dl(this%l-1)(['m', achar(abs(mod(iachar(this%cvar(1:this%l-2)) - iachar(that%cvar(1:that%l-2)), 96))+32)])
    print *, "binaryMinusL:", this%l, this%cvar, that%l, that%cvar, "->", binaryMinusL%l, binaryMinusL%cvar
  end function binaryMinusL

  function binaryStarL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l+2)) :: binaryStarL
    binaryStarL = dl(this%l+2)([achar(abs(mod(iachar(this%cvar) * iachar(that%cvar), 96))+32), 'n', 'o'])
    print *, "binaryStarL:", this%l, this%cvar, that%l, that%cvar, "->", binaryStarL%l, binaryStarL%cvar
  end function binaryStarL

  function binarySlashL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l-2)) :: binarySlashL
    binarySlashL = dl(this%l-2)(['p', achar(abs(mod(iachar(this%cvar(1:this%l-3)) / (mod(iachar(that%cvar(1:that%l-3)),8)+1), 96))+32)])
    print *, "binarySlashL:", this%l, this%cvar, that%l, that%cvar, "->", binarySlashL%l, binarySlashL%cvar
  end function binarySlashL

  function binaryPowerL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l+3)) :: binaryPowerL
    binaryPowerL = dl(this%l+3)([achar(abs(mod(iachar(this%cvar) ** mod(iachar(that%cvar),3), 96))+32), 'q', 'r', 's'])
    print *, "binaryPowerL:", this%l, this%cvar, that%l, that%cvar, "->", binaryPowerL%l, binaryPowerL%cvar
  end function binaryPowerL


  type(d2k(4,4)) function binaryPlus2K(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binaryPlus2K = d2k(4,4)(this%ivar + that%ivar, this%ivar2 + that%ivar2)
    print *, "binaryPlus2K:", this%ivar, this%ivar2, that%ivar, that%ivar2, "->", binaryPlus2K%ivar, binaryPlus2K%ivar2
  end function binaryPlus2K

  type(d2k(4,4)) function binaryMinus2K(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binaryMinus2K = d2k(4,4)(this%ivar - that%ivar, this%ivar2 - that%ivar2)
    print *, "binaryMinus2K:", this%ivar, this%ivar2, that%ivar, that%ivar2, "->", binaryMinus2K%ivar, binaryMinus2K%ivar2
  end function binaryMinus2K

  type(d2k(4,4)) function binaryStar2K(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binaryStar2K = d2k(4,4)(this%ivar * that%ivar, this%ivar2 * that%ivar2)
    print *, "binaryStar2K:", this%ivar, this%ivar2, that%ivar, that%ivar2, "->", binaryStar2K%ivar, binaryStar2K%ivar2
  end function binaryStar2K

  type(d2k(4,4)) function binarySlash2K(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binarySlash2K = d2k(4,4)(this%ivar / that%ivar, this%ivar2 / that%ivar2)
    print *, "binarySlash2K:", this%ivar, this%ivar2, that%ivar, that%ivar2, "->", binarySlash2K%ivar, binarySlash2K%ivar2
  end function binarySlash2K

  type(d2k(4,4)) function binaryPower2K(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binaryPower2K = d2k(4,4)(this%ivar ** that%ivar, this%ivar2 ** that%ivar2)
    print *, "binaryPower2K:", this%ivar, this%ivar2, that%ivar, that%ivar2, "->", binaryPower2K%ivar, binaryPower2K%ivar2
  end function binaryPower2K


  function binaryPlus2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+1)) :: binaryPlus2L
    binaryPlus2L = d2l(4,this%l+1)([achar(abs(mod(iachar(this%cvar) + iachar(that%cvar), 96))+32), 'Y'],this%ivar + that%ivar)
    print *, "binaryPlus2L:", this%l, this%cvar, this%ivar, that%l, that%cvar, that%ivar, "->", binaryPlus2L%l, binaryPlus2L%cvar, binaryPlus2L%ivar
  end function binaryPlus2L

  function binaryMinus2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l-1)) :: binaryMinus2L
    binaryMinus2L = d2l(4,this%l-1)(['Q', achar(abs(mod(iachar(this%cvar(1:this%l-2)) - iachar(that%cvar(1:that%l-2)), 96))+32)],this%ivar - that%ivar)
    print *, "binaryMinus2L:", this%l, this%cvar, this%ivar, that%l, that%cvar, that%ivar, "->", binaryMinus2L%l, binaryMinus2L%cvar, binaryMinus2L%ivar
  end function binaryMinus2L

  function binaryStar2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+2)) :: binaryStar2L
    binaryStar2L = d2l(4,this%l+2)([achar(abs(mod(iachar(this%cvar) * iachar(that%cvar), 96))+32), 'X', 'W'],this%ivar * that%ivar)
    print *, "binaryStar2L:", this%l, this%cvar, this%ivar, that%l, that%cvar, that%ivar, "->", binaryStar2L%l, binaryStar2L%cvar, binaryStar2L%ivar
  end function binaryStar2L

  function binarySlash2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l-2)) :: binarySlash2L
    binarySlash2L = d2l(4,this%l-2)(['P', achar(abs(mod(iachar(this%cvar(1:this%l-3)) / (mod(iachar(that%cvar(1:that%l-3)),8)+1), 96))+32)],this%ivar / that%ivar)
    print *, "binarySlash2L:", this%l, this%cvar, this%ivar, that%l, that%cvar, that%ivar, "->", binarySlash2L%l, binarySlash2L%cvar, binarySlash2L%ivar
  end function binarySlash2L

  function binaryPower2L(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+3)) :: binaryPower2L
    binaryPower2L = d2l(4,this%l+3)([achar(abs(mod(iachar(this%cvar) ** mod(iachar(that%cvar),3), 96))+32), 'V', 'U', 'T'],this%ivar ** that%ivar)
    print *, "binaryPower2L:", this%l, this%cvar, this%ivar, that%l, that%cvar, that%ivar, "->", binaryPower2L%l, binaryPower2L%cvar, binaryPower2L%ivar
  end function binaryPower2L


end module dtpUOpBinaryAssociationmod


program dtpUOpBinaryAssociation

  use dtpUOpBinaryAssociationmod
  implicit none

  type(dk(4)) :: xk4a, xk4b, xk4c, xk4d

  type(dl(5)) :: xla, xlb
  type(dl(6)) :: xlc
  type(dl(7)) :: xld, xlh
  type(dl(4)) :: xle
  type(dl(3)) :: xlf, xli
  type(dl(8)) :: xlg
  type(dl(9)) :: xlj
  type(dl(1)) :: xlk
  type(dl(11)) :: xll

  type(d2k(4,4)) :: x2k44a, x2k44b, x2k44c, x2k44d

  type(d2l(4,7))  :: x2l4a, x2l4b, x2l4m
  type(d2l(4,8))  :: x2l4c
  type(d2l(4,9))  :: x2l4d, x2l4h
  type(d2l(4,6))  :: x2l4e
  type(d2l(4,5))  :: x2l4f, x2l4i
  type(d2l(4,10)) :: x2l4g
  type(d2l(4,11)) :: x2l4j
  type(d2l(4,3))  :: x2l4k
  type(d2l(4,13)) :: x2l4l


  xk4a   = dk(4)(12)
  xk4b   = dk(4)(2)
  xk4c   = dk(4)(3)

  xla    = dl(5)(['e','d','c','b','a'])
  xlb    = dl(5)(['g','h','i','j','f'])
  xlc    = dl(6)(['m','k','l','o','n','p'])
  xld    = dl(7)(['s','t','u','q','v','r','w'])
  xle    = dl(4)(['a','y','z','x'])
  xlf    = dl(3)(['b','c','d'])
  xlg    = dl(8)(['e','f','g','h','i','j','k','l'])

  x2k44a = d2k(4,4)(10,14)
  x2k44b = d2k(4,4)(3,2)
  x2k44c = d2k(4,4)(2,3)

  x2l4a  = d2l(4,7)(['c','d','e','f','g','h','i'],13)
  x2l4b  = d2l(4,7)(['p','o','n','m','l','k','j'],2)
  x2l4c  = d2l(4,8)(['t','u','v','w','x','q','r','s'],4)
  x2l4d  = d2l(4,9)(['a','b','c','y','z','e','d','g','f'], 1017)
  x2l4e  = d2l(4,6)(['k','i','l','h','j','m'],9)
  x2l4f  = d2l(4,5)(['n','q','r','o','p'],2)
  x2l4g  = d2l(4,10)(['s','t','u','y','z','a','b','v','w','x'],13)
  x2l4m  = d2l(4,7)(['c','d','e','f','g','h','i'],3)

  xk4d  = xk4a+xk4b+xk4c   ! (12+2)+3 = 14 + 3 = 17
  xk4d  = (xk4a+xk4b)+xk4c

  xk4d  = xk4a-xk4b-xk4c   ! (12-2)-3 = 10 - 3 = 7
  xk4d  = (xk4a-xk4b)-xk4c

  xk4d  = xk4a*xk4b*xk4c   ! (12*2)*3 = 24 * 3 = 72
  xk4d  = (xk4a*xk4b)*xk4c

  xk4d  = xk4a/xk4b/xk4c   ! (12/2)/3 = 6 / 3 = 2
  xk4d  = (xk4a/xk4b)/xk4c

  xk4d  = xk4a**xk4b**xk4c   ! 12**(2**3) = 12 ** 8 = 429981696 (429,981,696)
  xk4d  = xk4a**(xk4b**xk4c)


  xk4d  = dk(4)(12)+dk(4)(2)+dk(4)(3)
  xk4d  = (dk(4)(12)+dk(4)(2))+dk(4)(3)

  xk4d  = dk(4)(12)-dk(4)(2)-dk(4)(3)
  xk4d  = (dk(4)(12)-dk(4)(2))-dk(4)(3)

  xk4d  = dk(4)(12)*dk(4)(2)*dk(4)(3)
  xk4d  = (dk(4)(12)*dk(4)(2))*dk(4)(3)

  xk4d  = dk(4)(12)/dk(4)(2)/dk(4)(3)
  xk4d  = (dk(4)(12)/dk(4)(2))/dk(4)(3)

  xk4d  = dk(4)(12)**dk(4)(2)**dk(4)(3)
  xk4d  = dk(4)(12)**(dk(4)(2)**dk(4)(3))


  xlh = xla+xlb+xlc ! 5+5+6 7
  xlh = (xla+xlb)+xlc

  xli = xla-xlb-xle ! 5-5-4 3
  xli = (xla-xlb)-xle

  xlj = xla*xlb*xld ! 5*5*7 9
  xlj = (xla*xlb)*xld

  xlk = xla/xlb/xlf ! 5/5/3 1
  xlk = (xla/xlb)/xlf

  xll = xlg**xla**xlb ! 8**5**5 11
  xll = xlg**(xla**xlb)


  xlh = dl(5)(['e','d','c','b','a'])+dl(5)(['g','h','i','j','f'])+dl(6)(['m','k','l','o','n','p']) ! 5+5+6 7
  xlh = (dl(5)(['e','d','c','b','a'])+dl(5)(['g','h','i','j','f']))+dl(6)(['m','k','l','o','n','p'])

  xli = dl(5)(['e','d','c','b','a'])-dl(5)(['g','h','i','j','f'])-dl(4)(['a','y','z','x']) ! 5-5-4 3
  xli = (dl(5)(['e','d','c','b','a'])-dl(5)(['g','h','i','j','f']))-dl(4)(['a','y','z','x'])

  xlj = dl(5)(['e','d','c','b','a'])*dl(5)(['g','h','i','j','f'])*dl(7)(['s','t','u','q','v','r','w']) ! 5*5*7 9
  xlj = (dl(5)(['e','d','c','b','a'])*dl(5)(['g','h','i','j','f']))*dl(7)(['s','t','u','q','v','r','w'])

  xlk = dl(5)(['e','d','c','b','a'])/dl(5)(['g','h','i','j','f'])/dl(3)(['b','c','d']) ! 5/5/3 1
  xlk = (dl(5)(['e','d','c','b','a'])/dl(5)(['g','h','i','j','f']))/dl(3)(['b','c','d'])

  xll = dl(8)(['e','f','g','h','i','j','k','l']) ** dl(5)(['e','d','c','b','a'])**dl(5)(['g','h','i','j','f']) ! 8**5**5 11
  xll = dl(8)(['e','f','g','h','i','j','k','l']) ** (dl(5)(['e','d','c','b','a'])**dl(5)(['g','h','i','j','f']))


  x2k44d  = x2k44a+x2k44b+x2k44c
  x2k44d  = (x2k44a+x2k44b)+x2k44c

  x2k44d  = x2k44a-x2k44b-x2k44c
  x2k44d  = (x2k44a-x2k44b)-x2k44c

  x2k44d  = x2k44a*x2k44b*x2k44c
  x2k44d  = (x2k44a*x2k44b)*x2k44c

  x2k44d  = x2k44a/x2k44b/x2k44c
  x2k44d  = (x2k44a/x2k44b)/x2k44c

  x2k44d  = x2k44a**x2k44b**x2k44c
  x2k44d  = x2k44a**(x2k44b**x2k44c)

  x2k44a  = d2k(4,4)(10,14)+d2k(4,4)(3,2)+d2k(4,4)(2,3)
  x2k44a  = (d2k(4,4)(10,14)+d2k(4,4)(3,2))+d2k(4,4)(2,3)

  x2k44a  = d2k(4,4)(10,14)-d2k(4,4)(3,2)-d2k(4,4)(2,3)
  x2k44a  = (d2k(4,4)(10,14)-d2k(4,4)(3,2))-d2k(4,4)(2,3)

  x2k44a  = d2k(4,4)(10,14)*d2k(4,4)(3,2)*d2k(4,4)(2,3)
  x2k44a  = (d2k(4,4)(10,14)*d2k(4,4)(3,2))*d2k(4,4)(2,3)

  x2k44a  = d2k(4,4)(10,14)/d2k(4,4)(3,2)/d2k(4,4)(2,3)
  x2k44a  = (d2k(4,4)(10,14)/d2k(4,4)(3,2))/d2k(4,4)(2,3)

  x2k44a  = d2k(4,4)(10,14)**d2k(4,4)(3,2)**d2k(4,4)(2,3)
  x2k44a  = d2k(4,4)(10,14)**(d2k(4,4)(3,2)**d2k(4,4)(2,3))


  x2l4h = x2l4a+x2l4b+x2l4c ! 7+7 +8 9
  x2l4h = (x2l4a+x2l4b)+x2l4c

  x2l4i = x2l4a-x2l4b-x2l4e ! 7-7 -6 5
  x2l4i = (x2l4a-x2l4b)-x2l4e

  x2l4j = x2l4a*x2l4b*x2l4d ! 7*7 *9 11
  x2l4j = (x2l4a*x2l4b)*x2l4d

  x2l4k = x2l4a/x2l4b/x2l4f ! 7/7 /5 3
  x2l4k = (x2l4a/x2l4b)/x2l4f

  x2l4l = x2l4g**x2l4b**x2l4m ! 7**7 **10 13
  x2l4l = x2l4g**(x2l4b**x2l4m)

  x2l4h = d2l(4,7)(['c','d','e','f','g','h','i'],13)+d2l(4,7)(['p','o','n','m','l','k','j'],2)+d2l(4,8)(['t','u','v','w','x','q','r','s'],4) ! 7+7 +8 9
  x2l4h = (d2l(4,7)(['c','d','e','f','g','h','i'],13)+d2l(4,7)(['p','o','n','m','l','k','j'],2))+d2l(4,8)(['t','u','v','w','x','q','r','s'],4)

  x2l4i = d2l(4,7)(['c','d','e','f','g','h','i'],13)-d2l(4,7)(['p','o','n','m','l','k','j'],2)-d2l(4,6)(['k','i','l','h','j','m'],9) ! 7-7 -6 5
  x2l4i = (d2l(4,7)(['c','d','e','f','g','h','i'],13)-d2l(4,7)(['p','o','n','m','l','k','j'],2))-d2l(4,6)(['k','i','l','h','j','m'],9)

  x2l4j = d2l(4,7)(['c','d','e','f','g','h','i'],13)*d2l(4,7)(['p','o','n','m','l','k','j'],2)*d2l(4,9)(['a','b','c','y','z','e','d','g','f'], 1017) ! 7*7 *9 11
  x2l4j = (d2l(4,7)(['c','d','e','f','g','h','i'],13)*d2l(4,7)(['p','o','n','m','l','k','j'],2))*d2l(4,9)(['a','b','c','y','z','e','d','g','f'], 1017)

  x2l4k = d2l(4,7)(['c','d','e','f','g','h','i'],13)/d2l(4,7)(['p','o','n','m','l','k','j'],2)/d2l(4,5)(['n','q','r','o','p'],2) ! 7/7 /5 3
  x2l4k = (d2l(4,7)(['c','d','e','f','g','h','i'],13)/d2l(4,7)(['p','o','n','m','l','k','j'],2))/d2l(4,5)(['n','q','r','o','p'],2)

  x2l4l = d2l(4,10)(['s','t','u','y','z','a','b','v','w','x'],3) ** d2l(4,7)(['p','o','n','m','l','k','j'],2) ** d2l(4,7)(['c','d','e','f','g','h','i'],3) ! 10 **7**7 13
  x2l4l = d2l(4,10)(['s','t','u','y','z','a','b','v','w','x'],3) ** (d2l(4,7)(['p','o','n','m','l','k','j'],2) ** d2l(4,7)(['c','d','e','f','g','h','i'],3))


end program dtpUOpBinaryAssociation
