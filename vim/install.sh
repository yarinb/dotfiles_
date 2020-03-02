#!/bin/sh
echo "VIM CONFIG"
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
vim +'PlugInstall --sync' +qa
vim +'PlugUpdate' +qa

if command -v nvim >/dev/null 2>&1; then
    echo "NVIM CONFIG"
	curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

	ln -sf "$DOTFILES/vim/vim.symlink" ~/.config/nvim
	nvim +'PlugInstall --sync' +qa
	nvim +'PlugUpdate' +qa
fi