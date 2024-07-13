N = 50;
r = normrnd(0,1,N);
bsMat = zeros(N);
for i = 1:N
    for j=1:N
        bsMat(i,j) = sum(r(1:i,1:j),'all');
    end
end
surf(bsMat)