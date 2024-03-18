module mod_atualizar
    
    implicit none
    
    contains
    
    subroutine interna(a, b, c, d, e, f)
    
    Real, intent(in) :: a(:), b(:), e(:,:) !fun��es ativadas e d
    Real, intent(in out) :: c(:,:), d(:) !Peso e bias
    Integer, intent(in) :: f !conjunto
    Integer, parameter :: alpha = 1
    Integer :: i, j, k
    
        do i = 1,size(b) !Indice de camada saida
            d(i) = d(i) + alpha * (e(f,i) - b(i)) * b(i) * (1 - b(i))
            do j = 1,size(a) !indice de camada de escondida
                c(j,i) = c(j,i) + alpha * (e(f,i) - b(i)) * a(j) * b(i) * (1 - b(i))
            end do
        end do
    
    end subroutine interna
    
    subroutine inicio(a, b, c, d, e, f, g, h)
    
    Real, intent(in) :: a(:), b(:), g(:,:), e(:,:), h(:,:) !fun��es ativadas, valores de entrada, d e peso da camada anteiror
    Real, intent(in out) :: c(:,:), d(:) !Peso e bias
    Integer, intent(in) :: f !conjunto
    Integer, parameter :: alpha = 1
    Integer :: i, j, k
    
        do i = 1,size(b) !Indice de camada saida
            do j = 1,size(a) !indice de camada de escondida
                d(j) = d(j) + alpha * b(i) * (1 - b(i)) * (e(f,i) - b(i)) * h(j,i) * a(j) * (1 - a(j))
                do k = 1,size(c(:,1)) !Indice camadas de entrada
                    c(k,j) = c(k,j) + alpha * b(i) * (1 - b(i)) * (e(f,i) - b(i)) * h(j,i) * a(j) * (1 - a(j)) * g(f,k)
                end do 
            end do
        end do
    
    end subroutine inicio
    
end module mod_atualizar