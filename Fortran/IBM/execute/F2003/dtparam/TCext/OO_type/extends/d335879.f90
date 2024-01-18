! Reduced code of F2003/dtparam/TCext/OO_type/extends/fext017.scenario
! to test (possibly) side-effect (linker error: Undefined symbol: .b1_m%type) 
! of defect 335879

module m
    type base(k)
        integer, kind :: k
        contains
        procedure, nopass :: type => baseType
    end type

    type (base(4)) :: b1_m

    contains

    integer function baseType ()
        baseType = 1
    end function

end module

program fext017
    use m

    print *, b1_m%type()
end
