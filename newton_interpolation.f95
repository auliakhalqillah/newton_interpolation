! HOW TO COMPILE THROUGH COMMAND LINE (CMD OR TERMINAL)
! gfortran -c newton_interpolation.f95
! gfortran -o newton_interpolation newton_interpolation.o
!
! The program is open source and can use to numeric study purpose.
! The program was build by Aulia Khalqillah,S.Si., M.Si
!
! email: auliakhalqillah.mail@gmail.com
! ==============================================================================
program newtonint
    implicit none
    
    integer, parameter :: N = 5
    integer :: nrow,ncol
    integer :: i,j,k
    real :: x(N),y(N), ms(N-1)
    real :: m(N,N)
    real :: xq, P

    ! Initial data
    x(1) = 0.0
    x(2) = 0.2
    x(3) = 0.4
    x(4) = 0.6
    x(5) = 0.8
    y(1) = 1.0
    y(2) = 1.2214
    y(3) = 1.4918
    y(4) = 1.8221
    y(5) = 2.2255

    ! Generate initial zero matrix NxN
    nrow = N
    ncol = N 
    do i = 1,nrow
        do j = 1,ncol
            m(i,j) = 0
            write(*,*) i,j,m(i,j)
        end do
    end do
    
    write(*,*)
    ! Save the y data to the first column of matrix as zero degree
    do i = 1,nrow
        m(i,1) = y(i)
    end do

    ! Check the current matrix
    do i = 1,nrow
        write(*,*) (m(i,j), j = 1,ncol)
    end do

    write(*,*)
    ! Calculate the data for remaining column of matrix by using y data 
    do j = 2,ncol 
        do i = j,nrow
            m(i,j) = (m(i,j-1) - m(i-1,j-1))/(x(i) - x(i-(j-1)))
        end do
    end do

    ! Check the current matrix
    do i = 1,nrow
        write(*,*) (m(i,j), j = 1,ncol)
    end do

    write(*,*)
    ! Calculate the newton interpolation 
    xq = 0.75
    ! Calculate multiplication iteration section
    ms(1) = (xq - x(1))
    do k = 1,n-1
        ms(k+1) = ms(k) * (xq - x(k+1))
    end do

    write(*,*)
    ! calculate the summation section
    P = 0
    do k = 2,n
        P = P + (m(k,k) * ms(k-1))
    end do

    P = y(1) + P
    write(*,*) P
end program newtonint