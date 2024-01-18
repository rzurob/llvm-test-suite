! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/dtio/d322506.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

module m
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: x(:)
    end type

    type base(k2,n2)    ! (4,20)
        integer, kind                      :: k2
        integer, len                       :: n2
        class(dataType(k2,:)), allocatable :: data
    end type
end module

program dcmlChildWrite005
use m
    logical(4), external :: precision_r4
    type(base(4,:)), allocatable :: b1(:)

    allocate (base(4,20) :: b1(12))

    do i = 1, 12, 3
        allocate(dataType(4,20) :: b1(i+2)%data)
        allocate(base(4,20)::b1(i+2)%data%x(1))

select type (x=>b1(i+2)%data%x(1)); type is(base(4,*)); allocate(dataType(4,x%n2) :: x%data); allocate(x%data%x(i+2), source=sin((/(j*1.0, j=1, i+2)/)))

            class default
                stop 30
        end select
    end do

    do i = 1, 12, 3
        select type (x => b1(i+2)%data%x(1))
            type is(base(4,*))
                select type (y => x%data%x)
                    type is (real)
                        do j = 1, size(y)
                            if (.not. precision_r4(y(j), sin(j*1.0))) then
                                print *, i, j, y(j), sin(j*1.0)

                                stop 1
                            end if
                        end do

                    class default
                        stop 20
                end select

            class default
                stop 10
        end select
    end do

end
