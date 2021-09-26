vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.o.spell = true

vim.o.clipboard = 'unnamedplus'

vim.o.termguicolors = true

vim.o.ruler = true
vim.o.showmode = true
vim.o.hlsearch = true

vim.o.relativenumber = true

vim.o.cursorline = true

vim.o.timeout = false
vim.o.ttimeout = false

vim.o.timeoutlen = 150

vim.o.showmatch = true

vim.o.smarttab = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4

vim.o.splitright = true
vim.o.splitbelow = true

vim.g.mapleader = ' '

vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end

vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	{
		'nvim-lua/plenary.nvim',
		lazy = false,
		priority = 1000,
	},
	{
		'catppuccin/nvim',
		name = 'catppuccin',
		lazy = false,
		config = function()
			vim.cmd.colorscheme('catppuccin-macchiato')
		end,
	},
	{
		'nvim-tree/nvim-web-devicons',
		lazy = true,
	},
	{
		'numToStr/Comment.nvim',
		dependencies = {
			'nvim-treesitter/nvim-treesitter',
			'JoosepAlviste/nvim-ts-context-commentstring',
		},
		config = function()
			require('Comment').setup {
				pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
			}
		end
	},
	{
		'alexghergh/nvim-tmux-navigation',
		opts = {},
		keys = {
			{ '<C-h>', '<cmd>lua require(\'nvim-tmux-navigation\').NvimTmuxNavigateLeft()<cr>' },
			{ '<C-j>', '<cmd>lua require(\'nvim-tmux-navigation\').NvimTmuxNavigateDown()<cr>' },
			{ '<C-k>', '<cmd>lua require(\'nvim-tmux-navigation\').NvimTmuxNavigateUp()<cr>' },
			{ '<C-l>', '<cmd>lua require(\'nvim-tmux-navigation\').NvimTmuxNavigateRight()<cr>' },
		},
	},
	{
		'ahmedkhalf/project.nvim',
		main = "project_nvim",
		opts = {
			patterns = { '>Sources' },
		},
	},
	{
		'lambdalisue/suda.vim',
		init = function()
			vim.g.suda_smart_edit = 1
		end,
	},
	{
		'hrsh7th/nvim-cmp',
		dependencies = {
			'windwp/nvim-autopairs',
			'hrsh7th/vim-vsnip',
			'hrsh7th/cmp-buffer',
			'hrsh7th/cmp-cmdline',
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-vsnip',
			'tzachar/cmp-tabnine',
		},
		config = function()
			local cmp_autopairs = require('nvim-autopairs.completion.cmp')
			local cmp = require('cmp')

			cmp.setup({
				snippet = {
					expand = function(args)
						vim.fn['vsnip#anonymous'](args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					['<C-b>'] = cmp.mapping.scroll_docs(-4),
					['<C-f>'] = cmp.mapping.scroll_docs(4),
					['<C-Space>'] = cmp.mapping.complete(),
					['<C-e>'] = cmp.mapping.abort(),
					['<CR>'] = cmp.mapping.confirm({ select = true }),
				}),
				sources = cmp.config.sources({
					{ name = 'cmp_tabnine' },
					{ name = 'nvim_lsp' },
					{ name = 'vsnip' },
					{ name = 'path' },
				}, {
					{ name = 'buffer' },
				})
			})

			cmp.setup.filetype('gitcommit', {
				sources = cmp.config.sources({
					{ name = 'cmp_git' },
				}, {
					{ name = 'buffer' },
				})
			})

			cmp.setup.cmdline({ '/', '?' }, {
				mapping = cmp.mapping.preset.cmdline(),
				sources = {
					{ name = 'buffer' }
				}
			})

			cmp.setup.cmdline(':', {
				mapping = cmp.mapping.preset.cmdline(),
				sources = cmp.config.sources({
					{ name = 'path' }
				}, {
					{ name = 'cmdline' }
				})
			})

			cmp.event:on(
				'confirm_done',
				cmp_autopairs.on_confirm_done()
			)
		end,
	},
	{
		'tzachar/cmp-tabnine',
		build = './install.sh',
	},
	{
		'AckslD/nvim-neoclip.lua',
		keys = {
			{ '<leader>p', '<cmd>Telescope neoclip<cr>' },
		},
	},
	{
		'TimUntersberger/neogit',
		dependencies = {
			'sindrets/diffview.nvim',
		},
		opts = {
			integrations = {
				diffview = true,
			},
		},
		keys = {
			{ '<leader>g', '<cmd>Neogit<cr>' }
		},
	},
	{
		'nvim-treesitter/nvim-treesitter',
		dependencies = {
			'nvim-treesitter/nvim-treesitter-textobjects',
			'windwp/nvim-ts-autotag',
			'andrewferrier/textobj-diagnostic.nvim',
			'szebniok/tree-sitter-wgsl',
			'HiPhish/nvim-ts-rainbow2',
			'JoosepAlviste/nvim-ts-context-commentstring',
		},
		build = ':TSUpdate',
		config = function()
			require('nvim-treesitter.configs').setup {
				ensure_installed = {
					'regex', 'bash', 'markdown', 'markdown_inline', 'lua', 'vim', 'rust', 'python', 'c', 'cpp',
					'c_sharp', 'gdscript', 'go', 'css', 'scss', 'html', 'json', 'javascript', 'typescript', 'wgsl',
				},
				highlight = {
					enable = true,
				},
				autotag = {
					enable = true,
				},
				rainbow = {
					enable = true,
					query = 'rainbow-parens',
					strategy = require('ts-rainbow').strategy.global,
				},
				context_commentstring = {
					enable = true,
				}
			}
		end
	},
	{
		'andrewferrier/textobj-diagnostic.nvim',
		opts = {}
	},
	{
		'szebniok/tree-sitter-wgsl',
		config = function()
			vim.filetype.add { extension = { wgsl = 'wgsl' } }

			local parser_config = require('nvim-treesitter.parsers').get_parser_configs()

			parser_config.wgsl = {
				install_info = {
					url = 'https://github.com/szebniok/tree-sitter-wgsl',
					files = { 'src/parser.c' }
				},
			}
		end
	},
	{
		'folke/trouble.nvim',
		opts = {},
	},
	{
		'nvim-telescope/telescope.nvim',
		lazy = false,
		dependencies = {
			'nvim-treesitter/nvim-treesitter',
			'folke/trouble.nvim',
			'ahmedkhalf/project.nvim',
			'rcarriga/nvim-notify',
			'AckslD/nvim-neoclip.lua',
		},
		keys = {
			{ '<leader>b',  '<cmd>Telescope buffers<cr>' },
			{ '<leader>ff', '<cmd>Telescope find_files<cr>' },
			{ '<leader>fg', '<cmd>Telescope live_grep<cr>' },
			{ '<leader>fh', '<cmd>Telescope help_tags<cr>' },
			{ '<leader>fp', '<cmd>Telescope projects<cr>' },
			{ '<leader>ft', '<cmd>Telescope treesitter<cr>' },
			{ '<leader>fs', '<cmd>Telescope lsp_workspace_symbols<cr>' },
			{ '<leader>fr', '<cmd>Telescope lsp_references<cr>' },
		},
		config = function()
			local telescope = require('telescope')
			local trouble = require('trouble')

			telescope.setup {
				pickers = {
					find_files = {
						find_command = { 'rg', '--ignore', '--hidden', '--files' }
					},
				},
				defaults = {
					mappings = {
						i = { ['<c-t>'] = trouble.open_with_trouble },
						n = { ['<c-t>'] = trouble.open_with_trouble },
					},
				},
			}

			telescope.load_extension('projects')
			telescope.load_extension("notify")
			telescope.load_extension("neoclip")
		end
	},
	{
		'folke/noice.nvim',
		dependencies = {
			'smjonas/inc-rename.nvim',
			'hrsh7th/nvim-cmp',
			'MunifTanjim/nui.nvim',
			'rcarriga/nvim-notify',
		},
		opts = {
			lsp = {
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true,
				},
			},
			presets = {
				bottom_search = true,
				command_palette = true,
				long_message_to_split = true,
				inc_rename = true,
				lsp_doc_border = false,
			},
		}

	},
	{
		'neovim/nvim-lspconfig',
		dependencies = {
			'lukas-reineke/lsp-format.nvim',
			'smjonas/inc-rename.nvim',
			'hrsh7th/cmp-nvim-lsp',
		},
		config = function()
			local function on_attach(client)
				require('lsp-format').on_attach(client)

				local bufopts = { noremap = true, silent = true, buffer = bufnr }

				vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
				vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
				vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
				vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
				vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
				vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bufopts)
				vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
				vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
				vim.keymap.set('n', '<leader>rn', ":IncRename ", bufopts)
				vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
				vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
				vim.keymap.set('n', '<leader><C-f>', function()
					vim.lsp.buf.format { async = true }
				end, bufopts)
			end

			local capabilities = require('cmp_nvim_lsp').default_capabilities()
			local s = { on_attach = on_attach, capabilities = capabilities }

			local lsp_servers = {
				'gopls', 'rust_analyzer', 'clangd', 'gdscript', 'vimls', 'lua_ls'
			}

			local lspc = require('lspconfig')

			for _, v in ipairs(lsp_servers) do
				lspc[v].setup(s)
			end
		end
	},
	{
		'rcarriga/nvim-notify',
		config = function()
			vim.notify = require("notify")

			vim.notify.Config = {
				fps = 60,
				stages = "fade",
				timeout = 5000,
			}
		end
	},
	{
		'windwp/nvim-autopairs',
		opts = {
			disable_in_replace_mode = false,
		},
	},
	{
		'jose-elias-alvarez/null-ls.nvim',
		config = function()
			local null_ls = require('null-ls')

			null_ls.setup {
				sources = {
					null_ls.builtins.formatting.beautysh,
					null_ls.builtins.formatting.cbfmt,
					null_ls.builtins.formatting.fish_indent,
					null_ls.builtins.formatting.fixjson,
					null_ls.builtins.formatting.gersemi,
					null_ls.builtins.formatting.isort,
					null_ls.builtins.formatting.nginx_beautifier,
					null_ls.builtins.formatting.remark,
					null_ls.builtins.formatting.taplo,
					null_ls.builtins.formatting.yamlfmt,
				},
			}
		end
	},
	{
		'smjonas/inc-rename.nvim',
		opts = {},
	},
	{
		'kylechui/nvim-surround',
		opts = {},
	},
	{
		'lukas-reineke/lsp-format.nvim',
		opts = {},
	},
	{
		'm-demare/hlargs.nvim',
		opts = {},
	},
	{
		'NvChad/nvim-colorizer.lua',
		opts = {},
	},
	{
		'ojroques/nvim-bufdel',
		keys = {
			{ '<leader>d', '<cmd>BufDel<cr>' }
		}
	},
	{
		'rcarriga/nvim-dap-ui',
		opts = {},
		dependencies = { 'mfussenegger/nvim-dap' },
		keys = {
			{ '<leader>D', '<cmd>lua require(\'dapui\').toggle()<cr>' },
		}
	},
	'chaoren/vim-wordmotion',
	'marrub--/vim-zscript',
	'jghauser/mkdir.nvim',
}, {
	dev = {
		path = "~/Sources",
	},
	checker = {
		enabled = true,
		frequency = 3600,
	},
})
