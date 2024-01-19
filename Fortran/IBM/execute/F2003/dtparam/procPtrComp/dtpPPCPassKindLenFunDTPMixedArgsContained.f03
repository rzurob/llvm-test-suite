!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : function pointers spec'd by contained procedures (PASS, mixed, kind+len)
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCPassKindSubContained (<-dtpPPCBasicPassKindSub<-dtpPPCBasicPassKindSub<-dtpPPCBasicPass)
!*
!*  DESCRIPTION
!*
!*  Create objects with different KIND and LEN params and invoke PASS functions on them.
!*  Procedures are specified using a reference to a contained procedure.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCPassKindLenFunDTPMixedArgsContainedmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     character(l)  :: chval2
     integer(k)    :: ival2
     procedure (pC1), pointer, pass(this) :: p1 => null()
     procedure (pC2), pointer, pass(this) :: p2 => null()
     class(dt(k,:)), pointer              :: nextItem => null()
   contains
     procedure, pass :: pC1
     procedure, pass :: pC2
     procedure, pass :: display1
     procedure, pass :: display2
  end type dt

contains

  recursive function pC1(this)
    class(dt(1,*)), intent(in) :: this
    character(:), pointer :: pC1, ch
    character(2*len(this%chval)) :: chTmp
    chTmp = this%chval // this%chval2
    if (associated(this%nextItem)) then
       ch => this%nextItem%pC1()
       allocate(pC1, source=chTmp // ch)
       deallocate(ch)
    else
       allocate(pC1, source=chTmp)
    end if
  end function pC1

  recursive integer(2) function pC2(this)
    class(dt(2,*)), intent(in)   :: this
    integer :: tmp
    pC2 = this%ival + this%ival2
    if (associated(this%nextItem)) pC2 = pC2 + this%nextItem%pC2()
  end function pC2

  recursive subroutine display1(this, prefix)
    class(dt(1,*)), intent(in) :: this
    integer, intent(in), optional :: prefix
    integer :: prefixWidth, i
    prefixWidth = 0
    if (present(prefix)) prefixWidth = prefix
    print *, (' ',i=1,prefixWidth), ">", this%chval, "/", this%chval2, "<", this%ival, this%ival2
    if (associated(this%nextItem)) call display1(this%nextItem, 1)
  end subroutine display1

  recursive subroutine display2(this, prefix)
    class(dt(2,*)), intent(in) :: this
    integer, intent(in), optional :: prefix
    integer :: prefixWidth, i
    prefixWidth = 0
    if (present(prefix)) prefixWidth = prefix
    print *, (' ',i=1,prefixWidth), ">", this%chval, "/", this%chval2, "<", this%ival, this%ival2
    if (associated(this%nextItem)) call display2(this%nextItem, 1)
  end subroutine display2

  recursive function catVal1(this)
    class(dt(1,*)), intent(in) :: this
    character(:), pointer :: catVal1, ch
    if (associated(this%nextItem)) then
       ch => this%nextItem%p1()
       allocate(catVal1, source= this%chval // ch)
       deallocate(ch)
    else
       allocate(catVal1, source = this%chval)
    end if
  end function catVal1

  recursive integer(2) function sumVal1(this)
    class(dt(2,*)), intent(in) :: this
    sumVal1 = this%ival
    if (associated(this%nextItem)) sumVal1 = sumVal1 + this%nextItem%p2()
  end function sumVal1

  recursive function catVal2(this)
    class(dt(1,*)), intent(in) :: this
    character(:), pointer :: catVal2, ch
    if (associated(this%nextItem)) then
       ch => this%nextItem%p1()
       allocate(catVal2, source= this%chval2 // ch)
       deallocate(ch)
    else
       allocate(catVal2, source = this%chval2)
    end if
  end function catVal2

  recursive integer(2) function sumVal2(this)
    class(dt(2,*)), intent(in) :: this
    sumVal2 = this%ival2
    if (associated(this%nextItem)) sumVal2 = sumVal2 + this%nextItem%p2()
  end function sumVal2


end module dtpPPCPassKindLenFunDTPMixedArgsContainedmod


program dtpPPCPassKindLenFunDTPMixedArgsContained

  use dtpPPCPassKindLenFunDTPMixedArgsContainedmod
  implicit none

  type(dt(1,:)), pointer :: t1ap, t1bp
  type(dt(1,0)), target  :: t10t
  type(dt(1,5)), target  :: t15t

  type(dt(2,:)), pointer :: t2ap, t2bp
  type(dt(2,3)), target  :: t23t
  type(dt(2,5)), target  :: t25t

  character(:), pointer  :: chp
  integer :: res

  allocate(dt(1,2) :: t1ap)
  t1ap = dt(1,2)('xx',127,'yy',-27,catVal1,sumVal1,t10t)
  t1bp => t15t
  t10t = dt(1,0)('', 100,'', -127, catVal1, sumVal1, t1bp)
  t15t = dt(1,5)('abcde', -100,'fghij', 17, catVal1, sumVal1) ! default nextItem is null()

  allocate(t2ap, source=dt(2,2)('xx',20000,'yy',-27,catVal2,sumVal2,t23t))
  t2bp => t25t
  t23t = dt(2,3)('klm', -1000,'nop', 1127, catVal2, sumVal2, t2bp)
  t25t = dt(2,5)('abcde', 1000,'fghij', 1117, catVal2, sumVal2) ! default nextItem is null()

  call t1ap % display1()
  call t2ap % display2()

  ! Invoke pC1 and pC2 on each list - should work without problems, invoking pC1 and pC2 on each item
  chp => t1ap % pC1() ! expect 'xx' // 'yy' // '' // '' // 'abcde' // 'fghij' == 'xxyyabcdefghij' -> len==14
  print *, len(chp), ">", chp, "<" ! expect: 14 >xxyyabcdefghij<
  deallocate(chp)

  res =  t2ap % pC2() ! expect 20000 + -27 + -1000 + 1127 + 1000 + 1117 = 22217
  print *, res        ! expect: 22217



  ! Invoke p1 and p2 on a list -- what is invoked at each step depends on what p1 and p2 point to.
  ! Note that the items in the t1ap list can only invoke p1, and the items in the t2ap list can only
  ! invoke p2.  At this stage, p1 points to catVal1 for every item in the t1ap list, and p2 points
  ! to sumVal2 for all the items in the t2ap list.

  chp => t1ap % p1() ! expect 'xx' // '' // 'abcde' == 'xxabcde' -> len==7
  print *, len(chp), ">", chp, "<" ! expect: 7 >xxabcde<
  deallocate(chp)

  res =  t2ap % p2() ! expect -27 + 1127 + 1117 = 2217
  print *, res       ! expect: 2217

  ! Slightly alter the list: now the first item in all lists points to the other procedure (catVal2
  ! instead of catVal1, sumVal1 instead of sumVal2); the outcome will change slightly.

  t1ap % p1 => catVal2
  t1ap % p2 => sumVal2

  t2ap % p1 => catVal1
  t2ap % p2 => sumVal1

  ! Make sure changing those pointers does not change output of pC1, pC2:
  chp => t1ap % pC1()
  print *, len(chp), ">", chp, "<"
  deallocate(chp)

  res =  t2ap % pC2()
  print *, res


  chp => t1ap % p1() ! expect 'yy' // '' // 'abcde' == 'yyabcde' -> len==7
  print *, len(chp), ">", chp, "<" ! expect: 7 >yyabcde<
  deallocate(chp)

  res =  t2ap % p2() ! expect 20000 + 1127 + 1117 = 22244
  print *, res       ! expect: 22244

  ! Now change the remaining pointers:
  t10t % p1 => catVal2
  t10t % p2 => sumVal2
  t15t % p1 => catVal2
  t15t % p2 => sumVal2

  t23t % p1 => catVal1
  t23t % p2 => sumVal1
  t25t % p1 => catVal1
  t25t % p2 => sumVal1


  ! Make sure changing those pointers does not change output of pC1, pC2:
  chp => t1ap % pC1()
  print *, len(chp), ">", chp, "<"
  deallocate(chp)

  res =  t2ap % pC2()
  print *, res

  chp => t1ap % p1() ! expect 'yy' // '' // 'fghij' == 'yyfghij' -> len==7
  print *, len(chp), ">", chp, "<" ! expect: 7 >yyfghij<
  deallocate(chp)

  res =  t2ap % p2() ! expect 20000 + -1000 + 1000 = 20000
  print *, res       ! expect: 20000

  print *, "done"

end program dtpPPCPassKindLenFunDTPMixedArgsContained
