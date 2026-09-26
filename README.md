# Pesquisa de Survey

Minhas anotações das aulas de Pesquisa de Survey (IESP-UERJ, 2025.1), disciplina de desenho, amostragem e análise de surveys com [Fernando Meireles](https://fmeireles.com/).

**Site:** [felipelamarca.com/Survey-Research](https://felipelamarca.com/Survey-Research/)

| Pasta | Conteúdo |
|:--|:--|
| `aulas/` | uma página por aula (`aula-NN.qmd`), com leituras anotadas e anotações de aula |
| `trabalhos/` | as quatro tarefas entregues (relatório, PDF, enunciado e dados) e a apresentação de Goplerud (2023), com os slides |
| `references.bib` | bibliografia da disciplina |
| `_freeze/` | resultados já computados, usados pelo CI |
| `_extensions/` | o template [Course Notes](https://github.com/felipelmc/Course-Notes-Template) |

Para renderizar localmente: instale os pacotes do `DESCRIPTION` e rode `quarto render`. Depois de mudar uma página com código R, faça o commit de `_freeze/` junto, porque o GitHub Actions publica sem executar R. Os slides são um projeto à parte: `quarto render trabalhos/apresentacao/slides`.

As anotações são pessoais e podem conter erros. Citações literais seguem o original, e trechos escritos com auxílio de IA aparecem em caixas identificadas.
