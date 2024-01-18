! GM DTP extension using:
! ftcx_dtp -ql -qdefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/ace/types/derived/acetdt01c.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-14 (original: 2006-09-08)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : derived type, defined assignment, elemental,
!*                               component
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Like acetdt01, we test defined assignment, but here it is a component which
!*  has defined assignment.  Thus, we should see its use even if the structure
!*  of which it is part appears in an array constructor.  So, if assignment is
!*  user-defined for type S, and one of the components of type T is of type S,
!*  then we should see defined assignment being invoked in:
!*
!*  t1 = t(s(val))
!*  t2 = t1
!*  t1array = t1
!*  t2array = t1array
!*
!*  *as well as* within the array constructors below:
!*
!*  t1array = (/ t1 /)
!*  t2array = (/ t1array /)
!*
!*  The tricky part is that defined assignment will be invoked once in some
!*  cases, and sometimes twice!  When an array or a structure is constructed,
!*  we may find defined assigment invoked; when the array or structure so
!*  constructed is then assigned, we will find the defined assignment invoked
!*  again!
!*
!*  There are several ways in which defined assignment can be set up:  by use of
!*  a generic binding to assignment(=) in the type definition, using type-bound
!*  procedures and passed-object dummy arguments, or by use of a generic
!*  interface and procedures (type-bound or otherwise) which do not have
!*  passed-object dummy arguments.
!*  C461 requires specific bindings for assignment(=) to have a passed-object
!*  dummy argument, which must be "a scalar, nonpointer, nonallocatable dummy
!*  data object", according to C453.  This means that we cannot define
!*  assignment for arrays except by using either *elemental* type-bound
!*  procedures with passed-object dummy arguments (so no print statements,
!*  etc.), or procedures with non-passed-object dummy array arguments and a
!*  generic interface.
!*
!*  Here we test type-bound procedures with passed-object dummy arguments,
!*  meaning we use elemental procedures, and cannot print trace output, but
!*  we can show that defined assignment has been used if we assign a different
!*  value than would be assigned by intrinsic assignment, e.g., by first
!*  doubling the value.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt01cl_dlp_dpvmod

  implicit none
  logical :: invalidDataEncountered = .false.

  type dt(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     integer(k1)   :: datum
   contains
     procedure :: dtAssignDt
     generic   :: assignment(=) => dtAssignDt
  end type dt

  type Container(l2,k2)    ! (20,4)
     integer, kind               :: k2
     integer, len                :: l2
     integer(k2)                 :: id = 0
     type(dt(:,k2)), allocatable :: d
  end type Container

contains

  elemental subroutine dtAssignDt(this, that)
    class (dt(*,4)), intent(inout) :: this
    class (dt(*,4)), intent(in) :: that
!    print *, "Assign 2*", that%datum, "(", (2*that%datum), ") to", this % datum
    this % datum = 2 * that % datum
  end subroutine dtAssignDt

  subroutine init(this, id, datum)
    class (Container(*,4)), intent(inout) :: this
    integer, intent(in) :: id, datum
    this % id = id
    allocate(dt(20,4) :: this % d)
    this % d % datum = datum
  end subroutine init

  subroutine validateFromSources(tgt, src1, src2)
    class (Container(*,4)), intent(in) :: tgt(2), src1, src2
    call validateFromSource(tgt(1), src1)
    call validateFromSource(tgt(2), src2)
  end subroutine validateFromSources

  subroutine validateFromSource(tgt, src)
    class (Container(*,4)), intent(in) :: tgt, src
    integer :: expectedID, expectedDatum
    expectedID = src % id
    expectedDatum = 2 * src % d % datum
    if (tgt % id /= expectedID .or. tgt % d % datum /= expectedDatum) then
       print *, "Unexpected results:", tgt % id, ",", tgt % d % datum, "should be", expectedID, ",", expectedDatum
       invalidDataEncountered = .true.
    endif
  end subroutine validateFromSource

end module acetdt01cl_dlp_dpvmod


program acetdt01cl_dlp_dpv

  use acetdt01cl_dlp_dpvmod
  implicit none

  type (Container(20,4)) :: cont, cont1, cont2, cont3
  type (Container(20,4)), target :: conta(2), conta2(2)
  type (Container(:,4)), allocatable :: contall(:)
  type (Container(:,4)), pointer :: contp(:)
  integer :: i

  call init(cont, 11,101)
  call init(cont2, 12,102)
  call init(cont3, 13,103)
  call init(conta(1), 14,104)
  call init(conta(2), 15,105)
  call init(conta2(1), 16,106)
  call init(conta2(2), 17,107)

  cont1 = Container(20,4)(19, dt(20,4)(109))

  conta  = (/ cont3, cont1 /)
  call validateFromSources(conta, cont3, cont1)

  conta2 = (/ conta /)
  call validateFromSources(conta2, conta(1), conta(2))

  conta  = (/ Container(20,4):: cont, cont2 /)
  call validateFromSources(conta, cont, cont2)

  conta2 = (/ Container(20,4):: conta /)
  call validateFromSources(conta2, conta(1), conta(2))


  conta  = (/ (cont3, cont1, i=1,1) /)
  call validateFromSources(conta, cont3, cont1)

  conta2 = (/ (conta, i=1,1) /)
  call validateFromSources(conta2, conta(1), conta(2))

  conta  = (/ Container(20,4):: (cont, cont2, i=1,1) /)
  call validateFromSources(conta, cont, cont2)

  conta2 = (/ Container(20,4):: (conta, i=1,1) /)
  call validateFromSources(conta2, conta(1), conta(2))


  allocate(Container(20,4) :: contall(2))
  call init(contall(1), 21,121)
  call init(contall(2), 22,122)

  conta = (/ Container(20,4):: contall /)
  call validateFromSources(conta, contall(1), contall(2))

  contall = (/ Container(20,4):: conta2 /)
  call validateFromSources(contall, conta2(1), conta2(2))

  conta = (/ Container(20,4):: (contall, i=1,1) /)
  call validateFromSources(conta, contall(1), contall(2))

  contall = (/ Container(20,4):: (cont3, cont1, i=1,1) /)
  call validateFromSources(contall, cont3, cont1)

  contp => conta
  contp = (/ Container(20,4):: contall /)
  call validateFromSources(contp, contall(1), contall(2))
  call validateFromSources(conta, contall(1), contall(2))

  contp = cont2
  contp = (/ Container(20,4):: (contall, i=1,1) /)
  call validateFromSources(contp, contall(1), contall(2))

  if (invalidDataEncountered) error stop 2

end program acetdt01cl_dlp_dpv
