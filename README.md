# CMChatAPI

## Identificação

Nome: Michel Augusto Altmann <br>
Curso: Sistemas de Informação

## Objetivo do projeto

O objetivo deste projeto é fazer toda a comunicação com o aplicativo CMChat que ja desenvolvi préviamente, toda a comunicação necessária ja existia em uma API que desenvolvi em JavaScript com o Express, então esse projeto é uma renovação da lógica que utilizei antes, com melhorias na arquitetura do código, separando as partes lógicas e banco de dados do escopo principal dos endpoints(rotas da api).

## Desenvolvimento

No início do desenvolvimento foi extremamente difícil converter a lógica que eu havia usado em outra linguagem para Haskell, sendo bem complicado de se acostumar com a sintaxe da linguagem, e com uma extrema dificuldade em utilizar os tipos (`Types.hs`) que eu criei, pois eu não esperava que receberia tantos erros de ambiguidade, por criar tipos diferentes com campos de mesmo nome.

Também tive dificuldade para utilizar o banco de dados, pois de início eu queria utilizar da mesma maneira que fazia no outro projeto, com um servidor (MySQL Server) dedicado a banco de dados e criando as tabelas utilizando o MySQL Workbench com o servidor em haskell apenas fazendo requests de queries SQL, porém, ao procurar uma biblioteca para realizar a comunicação com o servidor, todas estavam dando erro na hora de instalar, após várias tentativas de utilizar bibliotecas diferentes, sempre dava erro, provavelmente por estar utilizando um macbook com processador ARM, então acabei optando por utilizar do `sqlite-simple` mesmo.

Após conseguir realizar as primeiras execuções com o banco de dados e API funcionando com a ajuda do ChatGPT e Claude Sonnet 4, o percorrer do trabalho ficou bem mais tranquilo, pois ao saber como funcionava a sintaxe que eu precisava utilizar, só me foi preciso aplicar a lógica que eu ja sabia  nos outros endpoints que eu precisava fazer.

As funções mais difíceis foram aquelas que tratam da imagem, que sem a ajuda das IAs eu não sei se teria conseguido fazer, pois por mais que eu ja tenha feito em javascript, eu não fazia ideia de quais bibliotecas eu precisaria para fazer o tratamento do upload da imagem, as funções para criar pastas, mexer no caminho da imagem, remover a imagem, ou a verificação se ela existe.

Depois de progredir mais em relação ao código da API, percebi que não estava do jeito que eu queria estruturalmente, pois por mais que estivesse funcionando, assim como fiz anteriormente, estava tudo no mesmo arquivo `Api.hs`, tendo tratamento lógico e de banco de dados no mesmo arquivo onde tem os endpoints, sendo muito confuso de trabalhar. Ja que por mais que ja tivesse o `Handlers.hs`, eles estavam apenas fazendo a chamada do banco de dados e retornando o resultado para a API. <br>
Assim resolvi migrar toda a lógica que não tratava do Scotty (Biblioteca que trata das chamadas da API) para os seus respectivos Handlers, e fazendo o tratamento dos erros neles, assim as rotas da API tratam somente da entrada e saída de dados, mantendo um código mais limpo e fácil de entender. 

Desse jeito meu projeto ficou bem estruturado, com 3 arquivos principais, `Api.hs` tratando das rotas, `Handlers.hs` tratando da lógica necessária pela API e chamando o `Database.hs` que trata todo chamado diretamente com o banco de dados.

Até agora tudo que eu citei é relacionado diretamente à API e suas rotas, o desenvolvimento da comunicação em tempo real foi outro problema que tive que enfrentar devido à minha escolha pro trabalho, pois na minha aplicação prévia eu estava utilizando Socket.IO para a comunicação em tempo real, porém, atualmente essa biblioteca ta cada vez mais defazada, com sua versão mais atual em haskell sendo de 2020, gerando problemas na instalação devido a diferença de versões, sendo assim, optei por migrar meu código antigo para WebSockets na versão mobile, logo, mantendo a mesma implementação em haskell tambem.

Assim, com todo o código principal funcionando, o aplicativo está fazendo 100% das funções que fazia anteriormente, desde o registro de novo usuário e edição de seus dados, até o envio de imagens em um chat em tempo real.

### Alguns erros

- Erro de ambiguidade

~~~
/Users/michel/Documents/GitHub/perso-2025b-MichelAltmann/src/database/Database.hs:65:7: error: [GHC-87543] Ambiguous occurrence ‘username’. It could refer to either the field ‘username’ of record ‘NewUser’, imported from ‘Types’ at src/database/Database.hs:10:30-41, or the field ‘username’ of record ‘User’, imported from ‘Types’ at src/database/Database.hs:10:44-52, or the field ‘username’ of record ‘EditUser’, imported from ‘Types’ at src/database/Database.hs:10:15-27. 
...
~~~
Resolvido adicionando a tag `{-# LANGUAGE DuplicateRecordFields #-}` para permitir a repetição e utilizando "Pattern Matching" para extrair os campos necessários sem o haskell se perder (fonte: ChatGPT)

- Build do mysql-simple dando errado
~~~
  While building package mysql-0.2.1 (scroll up to its section to see the error) using: /private/var/folders/bm/9j5n6h7x44gg_1hhk9ttvygr0000gn/T/stack-47d25aecc91ca881/mysql-0.2.1/.stack-work/dist/aarch64-osx/ghc-9.10.2/setup/setup --verbose=1 --builddir=.stack-work/dist/aarch64-osx/ghc-9.10.2 configure --with-ghc=/Users/michel/.stack/programs/aarch64-osx/ghc-9.10.2/bin/ghc-9.10.2 --with-ghc-pkg=/Users/michel/.stack/programs/aarch64-osx/ghc-9.10.2/bin/ghc-pkg-9.10.2 --user --package-db=clear --package-db=global --package-db=/Users/michel/.stack/snapshots/aarch64-osx/
  ... 
  
  ...--dependency=containers=containers-0.7-ea84 -f-developer --exact-configuration --ghc-option=-fhide-source-paths Process exited with code: ExitFailure 1
~~~
Resolvido migrando para `sqlite-simple` ao invés de utilizar o MySQL Server


## Instalação e execução

### Pré-requisitos

1. **Instalar Stack** (gerenciador de projetos Haskell):
   ```bash
   # macOS (usando Homebrew)
   brew install haskell-stack
   
   # Linux
   curl -sSL https://get.haskellstack.org/ | sh
   
   # Windows
   # Baixar do site oficial: https://docs.haskellstack.org/en/stable/install_and_upgrade/
   ```

2. **Verificar instalação**:
   ```bash
   stack --version
   ```

### Rodando o projeto

1. **Clone o repositório**:
   ```bash
   git clone https://github.com/elc117/perso-2025b-MichelAltmann.git
   cd perso-2025b-MichelAltmann
   ```

2. **Instalar dependências e compilar**:
   ```bash
   stack build
   ```
   > Este comando pode demorar na primeira vez, pois irá baixar o GHC e todas as dependências automaticamente.

3. **Executar o servidor**:
   ```bash
   stack run
   ```

4. **Testar se está funcionando**:
   - Abra o navegador em `http://localhost:3000`
   - O WebSocket estará disponível em `ws://localhost:9160`

5. **Executar os testes** (opcional):
   ```bash
   stack test
   ```

### O que acontece quando roda

O servidor irá executar em duas portas:
- **Porta `3000`**: API REST (endpoints HTTP) para operações como registro, login, upload de imagens
- **Porta `9160`**: WebSocket server para comunicação em tempo real (chat)

### Estrutura do banco de dados

O projeto usa SQLite e cria automaticamente o arquivo `chatapp.db` na raiz do projeto com as tabelas necessárias.

O projeto está usando as seguintes bibliotecas:
|Pacote|Função|
|:-------|:-------|
|base|Biblioteca padrão do Haskell|
|websockets|Implementação de WebSockets para comunicação em tempo real|
|stm|Software Transactional Memory para concorrência thread-safe|
|containers|Estruturas de dados eficientes (Map, Set, etc.)|
|scotty|Framework web leve para definir rotas de APIs|
|http-types|Tipos e utilitários para status HTTP|
|sqlite-simple|Interface simples para SQLite em Haskell|
|filepath|Utilitários para manipulação de caminhos de arquivos|
|wai|Web Application Interface. Base usada pelo Scotty|
|random|Funções para gerar números aleatórios|
|wai-extra|Utilitários extras para Wai|
|wai-middleware-static|Middleware para servir arquivos estáticos como imagens|
|bytestring|Manipulação eficiente de dados binários e texto|
|warp|Servidor HTTP de alta performance|
|directory|Funções para trabalhar com o sistema de arquivos|
|text|Substituto eficiente para `String`|
|aeson|Biblioteca para parsing e encoding JSON (`FromJSON` / `ToJSON`)|
|time|Manipulação de data e hora|

## Resultado Final



https://github.com/user-attachments/assets/50ee8113-1145-4045-9d22-e797f868c34c



## Referências

- https://hackage.haskell.org/packages/search?terms=socket.io - Ultima versão do Socket.IO em 2020

### Alguns prompts que fiz pro Github Copilot
- "give me an example, dont change my code, of the createFriendRequest error handling using Database, Handlers and API"
- "can you show me how the Socket.IO implementation in haskell should look like?"
- "can I use websockets easily with socket IO in my android?"
- "what would be the best option, regardless of more work"
- "give me an example of how I can treat errors in my handlers, just like createFriendRequestHandler, but in cases that I need to return a user for example, how can I treat the error in handler and return said error or the user to the api? dont change my code"
- "how can I make the handler get the files correctly, dont change my code"
- "I want to make automated tests on all my handlers, give me an example on one of them, dont change my files"
