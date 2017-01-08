var app = Elm.Main.embed(
  document.getElementById("elm-app")
);

var searchGithub = (query) => {
  fetch(query)
    .then(response => response.json())
    .then(repos => app.ports.responseFromGithubApiWithJS.send(repos));
};

app.ports.searchGithubApiWithJS.subscribe(searchGithub);
