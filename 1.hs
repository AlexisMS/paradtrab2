import System.IO
import System.Directory

type Servico = (Int, String, Int, Float)


servicoGetCod :: Servico -> Int
servicoGetCod (i,_,_,_) = i

servicoGetDesc :: Servico -> String
servicoGetDesc (_,i,_,_) = i

servicoGetPrest :: Servico -> Int
servicoGetPrest (_,_,i,_) = i

servicoGetPreco :: Servico -> Float
servicoGetPreco (_,_,_,i) = i

--recebe dados de um novo servico
newServico :: IO()
newServico = do
    print ("[NOVO SERVICO]")
    print ("Codigo:")
    codigoString <- getLine
    let codigo = (read codigoString :: Int)
    print ("Nome:")
    nome <- getLine
    print ("Quantidade de Prestacoes:")
    quantidadeString <- getLine
    let quantidade = (read quantidadeString :: Int)
    print ("Preco por Hora:")
    precoString <- getLine
    let preco = (read precoString :: Float)
    saveServico(codigo, nome, quantidade, preco)

--adiciona um novo servico no banco de dados
saveServico :: Servico -> IO()
saveServico s = do
    out <- openFile "servico.db" AppendMode
    hPutStrLn out (show s)
    hClose out

--recebe do usuario a escolha de qual servico remover, recria o arquivo usando deleteServicoFromID e deleta o antigo
deleteServico :: IO()
deleteServico = do
    inp <- openFile "servico.db" ReadMode
    openServico inp
    print("ID do servico a ser removido:")
    deleteidstr <- getLine
    let deleteid = (read deleteidstr :: Int)
    hClose inp
    renameFile "servico.db" "oldservico.db"
    inp <- openFile "oldservico.db" ReadMode
    out <- openFile "servico.db" AppendMode
    deleteServicoFromID deleteid out inp
    hClose inp
    hClose out
    removeFile "oldservico.db"


--recria o arquivo de servicos sem o ID escolhido
deleteServicoFromID :: Int -> Handle -> Handle -> IO()
deleteServicoFromID id out inp =
    do endoffile <- hIsEOF inp
       if endoffile
            then return ()
            else do
                inpString <- hGetLine inp
                let select = (read inpString :: Servico)
                if (servicoGetCod select)==id
                    then print("feito")
                    else hPutStrLn out (show select)
                deleteServicoFromID id out inp

--printa uma lista de todos os servicos salvos
openServico :: Handle -> IO()
openServico inp = 
    do inpeof <- hIsEOF inp
       if inpeof
            then return ()
            else do
                inpString <- hGetLine inp
                print (inpString)
                openServico inp

--inicia o processo de alteracao de um servico
changeServico :: IO()
changeServico = do
    inp <- openFile "servico.db" ReadMode
    openServico inp
    print("ID do servico a ser alterado:")
    changeidstr <- getLine
    let changeid = (read changeidstr :: Int)
    hClose inp
    inp <- openFile "servico.db" ReadMode
    selectServicoFromID changeid inp

--apartir de um id, encontra o servico para alteracao
selectServicoFromID :: Int -> Handle -> IO()
selectServicoFromID id inp =
    do endoffile <- hIsEOF inp
       if endoffile
            then return ()
            else do
                inpString <- hGetLine inp
                let select = (read inpString :: Servico)
                if (servicoGetCod select)==id
                    then changeSelectedServico select inp
                    else selectServicoFromID id inp

--obtem os novos dados com o usuario e atualiza o banco de dados
changeSelectedServico :: Servico -> Handle -> IO()
changeSelectedServico serv h = do
    print ("[SERVICO SELECIONADO:]")
    print(serv)
    print ("Novo Codigo:")
    codigoString <- getLine
    let codigo = (read codigoString :: Int)
    print ("Novo Nome:")
    nome <- getLine
    print ("Nova Quantidade de Prestacoes:")
    quantidadeString <- getLine
    let quantidade = (read quantidadeString :: Int)
    print ("Novo Preco por Hora:")
    precoString <- getLine
    let preco = (read precoString :: Float)
    hClose h
    let deleteid = (servicoGetCod serv)
    renameFile "servico.db" "oldservico.db"
    inp <- openFile "oldservico.db" ReadMode
    out <- openFile "servico.db" AppendMode
    deleteServicoFromID deleteid out inp
    hClose inp
    hClose out
    removeFile "oldservico.db"
    saveServico(codigo, nome, quantidade, preco)

type Cliente = (Int, String, String, Int, String)

clienteGetCod :: Cliente -> Int
clienteGetCod (i,_,_,_,_) = i

--recebe do usuario os dados de um novo cliente
newCliente :: IO()
newCliente = do
    print ("[NOVO CLIENTE]")
    print ("Codigo:")
    codigoString <- getLine
    let codigo = (read codigoString :: Int)
    print ("Nome:")
    nome <- getLine
    print ("Cidade:")
    cidade <- getLine
    print ("Idade:")
    idadeString <- getLine
    let idade = (read idadeString :: Int)
    print ("Genero (M/F):")
    genero <- getLine
    saveCliente(codigo, nome, cidade, idade, genero)

--salva um novo cliente no banco de dados
saveCliente :: Cliente -> IO()
saveCliente c = do
    out <- openFile "cliente.db" AppendMode
    hPutStrLn out (show c)
    hClose out

--recebe do usuario a escolha de qual cliente remover, recria o arquivo usando deleteClienteFromID e deleta o antigo
deleteCliente :: IO()
deleteCliente = do
    inp <- openFile "cliente.db" ReadMode
    openCliente inp
    print("ID do cliente a ser removido:")
    deleteidstr <- getLine
    let deleteid = (read deleteidstr :: Int)
    hClose inp
    renameFile "cliente.db" "oldcliente.db"
    inp <- openFile "oldcliente.db" ReadMode
    out <- openFile "cliente.db" AppendMode
    deleteClienteFromID deleteid inp out
    hClose inp
    hClose out
    removeFile "oldcliente.db"

--recria o arquivo de clientes sem o ID escolhido
deleteClienteFromID :: Int -> Handle -> Handle -> IO()
deleteClienteFromID id inp out =
    do inpeof <- hIsEOF inp
       if inpeof
            then return()
            else do
                inpString <- hGetLine inp
                let select = (read inpString :: Cliente)
                if (clienteGetCod select)==id
                    then print("feito")
                    else hPutStrLn out inpString
                deleteClienteFromID id inp out

--printa uma lista de todos os clientes salvos
openCliente :: Handle -> IO()
openCliente h = 
    do heof <- hIsEOF h
       if heof
            then return()
            else do
                inpString <- hGetLine h
                print(inpString)
                openCliente h

--inica o processo de alteracao de um cliente
changeCliente :: IO()
changeCliente = do
    inp <- openFile "cliente.db" ReadMode
    openCliente inp
    print("ID do cliente a ser alterado:")
    changeidstr <- getLine
    let changeid = (read changeidstr :: Int)
    hClose inp
    inp <- openFile "cliente.db" ReadMode
    selectClienteFromID changeid inp

--apartir de um ID, encontra o cliente para alteracao
selectClienteFromID :: Int -> Handle -> IO()
selectClienteFromID id inp = 
    do endoffile <- hIsEOF inp
       if endoffile
            then return ()
            else do
                inpString <- hGetLine inp
                let select = (read inpString :: Cliente)
                if (clienteGetCod select)==id
                    then changeSelectedCliente select inp
                    else selectClienteFromID id inp

--obtem os novos dados com o usuario e atualiza o banco de dados
changeSelectedCliente :: Cliente -> Handle -> IO()
changeSelectedCliente cli h = do
    print("[CLINTE SELECIONADO:]")
    print(cli)
    print("Novo Codigo:")
    codigoString <- getLine
    let codigo = (read codigoString :: Int)
    print("Novo Nome:")
    nome <- getLine
    print("Nova Cidade:")
    cidade <- getLine
    print("Nova Idade:")
    idadeString <- getLine
    let idade = (read idadeString :: Int)
    print("Genero (M/F):")
    genero <- getLine
    let deleteid = (clienteGetCod cli)
    renameFile "cliente.db" "oldcliente.db"
    inp <- openFile "oldcliente.db" ReadMode
    out <- openFile "cliente.db" AppendMode
    deleteClienteFromID deleteid inp out
    hClose inp
    hClose out
    removeFile "oldcliente.db"
    saveCliente(codigo, nome, cidade, idade, genero)

--apresenta um menu principal
main :: IO ()
main = do
    print ("Escolha uma opcao:")
    print ("1 - adicionar servico")
    print ("2 - remover servico")
    print ("3 - alterar servico")
    print ("4 - adicionar cliente")
    print ("5 - remover cliente")
    print ("6 - alterar cliente")
    inpString <- getLine
    let inp = (read inpString :: Int)
    if (inp==1)
        then newServico
        else if (inp==2)
            then deleteServico
            else if (inp==3)
                then changeServico
                else if (inp==4)
                    then newCliente
                    else if (inp==5)
                        then deleteCliente
                        else if (inp==6)
                        then changeCliente
                        else do print("comando invalido")