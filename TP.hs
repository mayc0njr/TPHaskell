type Nome = String
type Preco = Int
type Codigo = Int
type Produto = (Nome,Preco)
type Produtos = [(Codigo,Nome,Preco)]
type Carrinho = [Codigo]
type Conta = [(Nome,Preco)]

--Quantidade de caracteres que serao impressos por linha.
tamLinha::Int
tamLinha = 30

--Caractere separador entre o nome e o preco do produto.
charLinha::Char
charLinha = '.'

--Caractere separador os reais e centavos.
charMoney::Char
charMoney = ','

--Texto exibido para mostrar o total da compra.
totaltxt::String
totaltxt = "Total"

--Nome do Supermercado.
marketName::String
marketName = "Mercadinho Big Bom"

--Banco de Dados contendo os produtos.
tabelaProdutos :: Produtos
tabelaProdutos = [(1,"Bixcoito",1010)
                 ,(2,"Pamonha",550)
                 ,(3,"Chiclete",50)
                 ,(4,"Banana",350)
                 ,(5,"Leite",250)
                 ,(5,"Pera",350)]

--Obtem um valor inteiro correspondente ao preco e separa os reais e centavos em uma String.
formataCentavos::Preco -> String
formataCentavos x
    |z < 10 = show y ++ charMoney:'0':(show z)
    |otherwise = show y ++ charMoney:(show z)
    where
        z = mod x 100
        y = div x 100

--Recebe um Produto, e retorna uma String no formato adequado com o nome e preco
--alinhados respectivamente a esquerda e a direita.
formataLinha::Produto-> String
formataLinha (x,y) = x ++ (replicate (tamLinha-t) charLinha) ++ z 
    where
        z = formataCentavos y
        t = (length x) + (length z)

--Recebe toda uma compra e formata linha por linha, alinhando os nomes e precos.
formataLinhas::Conta -> String
formataLinhas [] = []
formataLinhas (x:xs) = formataLinha x ++ '\n':(formataLinhas xs)

--Formata o valor total, no mesmo formato das linhas de produtos.
formataTotal::Preco -> String
formataTotal x = "\n"++formataLinha (totaltxt,x)

--Calcula o custo total da compra.
calculoTotal::Conta -> Preco
calculoTotal [] = 0
calculoTotal ((n,p):xs) = p + (calculoTotal xs)

--Formata toda a conta, alinhando nomes e precos a esquerda e direita respectivamente.
formataConta::Conta -> String
formataConta [] = []
formataConta (x:xs) = "\n"++center marketName++"\n"++formataLinhas (x:xs) ++ formataTotal (calculoTotal (x:xs)) ++ "\n"

--Retorna o mesmo texto centralizado.
center::Nome->Nome
center x = replicate (div (30 - length x) 2) ' ' ++ x

--Retorna o mesmo texto alinhado a direita.
right::Nome->Nome
right x = replicate (30 - length x) ' ' ++ x

--Procura um codigo na lista de produtos e retorna o produto correspondente.
procuraCodigo::Produtos->Codigo->Produto
procuraCodigo [] x = error "produto nao encontrado!"
procuraCodigo ((xc,xn,xp):xt) y
    | y == xc = (xn,xp)
    | otherwise = procuraCodigo xt y

--Procura por todos os codigos no banco de produtos e retorna uma lista de produto.
criaConta::Produtos->Carrinho->Conta
criaConta x y = map (procuraCodigo x) y

--Retorna a "nota fiscal" da compra, ou seja uma String contendo todos os produtos e precos formatados incluindo o total.
fazCompra::Produtos->Carrinho->String
fazCompra x y = formataConta (criaConta x y)

--Imprime a "nota fiscal" da compra, ou seja uma String contendo todos os produtos e precos formatados incluindo o total.
comprar::Produtos->Carrinho->IO ()
comprar x y = putStr(fazCompra x y)