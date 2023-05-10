-----------------------------------------------------------------------------------------
--                                                                                     --
--  nonk123's init.lua (tm)                                                            --
--                                                                                     --
--  ✅ Approved by LocalInsomniac and the rest of the Schwungus Software corporation.  --
--                                                                                     --
--         This init-file is dedicated to jrb0001. Rest in peace, jerbie.              --
--                                                                                     --
--                           Even though you aren't dead                               --
--                                     ...yet                                          --
--                                                                                     --
-----------------------------------------------------------------------------------------

-- Most accessed configuration entries.

KEYS = {
	{ '<leader>x', '<cmd>close<cr>' },
	{ '<leader>h', '<cmd>nohlsearch<cr>' },
}

GLOBALS = {
	loaded_netrw = 1,
	loaded_netrwPlugin = 1,

	mapleader = ' ',
}

OPTS = {
	spell = true,

	wrap = true,
	linebreak = true,

	clipboard = 'unnamedplus',

	termguicolors = true,

	ruler = true,
	showmode = true,
	hlsearch = true,

	relativenumber = true,

	cursorline = true,

	timeout = false,
	ttimeout = false,

	showmatch = true,

	smarttab = true,
	tabstop = 4,
	shiftwidth = 4,

	splitright = true,
	splitbelow = true,
}

TREESITTER_PARSERS = {
	'regex', 'bash', 'markdown', 'markdown_inline', 'lua', 'vim', 'rust', 'python', 'c', 'cpp', 'c_sharp', 'gdscript',
	'go', 'css', 'scss', 'html', 'json', 'javascript', 'typescript', 'wgsl',
}

-- TODO: perhaps install mason and make it support this list?
LSP_SERVERS = {
	'gopls', 'rust_analyzer', 'clangd', 'gdscript', 'vimls', 'lua_ls', 'marksman'
}

NULL_LSP_PROGRAMS = {
	'beautysh', 'fish_indent', 'fixjson', 'gersemi', 'isort', 'nginx_beautifier', 'remark', 'taplo', 'yamlfmt',
}

-- Now, for the actual initialization...

for k, v in pairs(GLOBALS) do
	vim.g[k] = v
end

for k, v in pairs(OPTS) do
	vim.opt[k] = v
end

-- Lazy package-manager installation.

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'

if not vim.loop.fs_stat(lazypath) then
	vim.fn.system {
		'git',
		'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable',
		lazypath,
	}
end

vim.opt.rtp:prepend(lazypath)

-- LOTS of plugin-specific configuration. This isn't meant to be changed too often.

require('lazy').setup({
	-----------------------------------------
	-- /!\ EXTREMELY IMPORTANT PLUGINS /!\ --
	-----------------------------------------

	-- Required by various plugins. Also used to set up the keybindings.
	{
		'nvim-lua/plenary.nvim',
		lazy = false,
		priority = 1002,
		-- Putting my keybindings here because `keys` is easier to use than `vim.keymap.set`.
		keys = KEYS,
	},

	-- I want to track my coding time at any cost.
	{
		'wakatime/vim-wakatime',
		lazy = false,
		priority = 1001,
	},

	-- THE color scheme. Period.
	{
		'catppuccin/nvim',
		name = 'catppuccin',
		lazy = false,
		priority = 1000,
		config = function()
			vim.cmd.colorscheme('catppuccin-macchiato')
		end,
	},

	-- The CHAD tree.
	{
		'ms-jpq/chadtree',
		branch = 'chad',
		build = {
			'python3 -m chadtree deps',
			':CHADdeps',
		},
		keys = {
			{ '<leader>v', '<cmd>CHADopen<cr>' },
		}
	},

	--------------------------------------------------------
	-- All the other plugins. Nothing special about them. --
	--------------------------------------------------------

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
		'lukas-reineke/indent-blankline.nvim',
		lazy = false,
		opts = {
			space_char_blankline = ' ',
			show_current_context = true,
			show_current_context_start = true,
		}
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
		"folke/todo-comments.nvim",
		lazy = false,
		dependencies = {
			'folke/trouble.nvim',
			'nvim-telescope/telescope.nvim',
		},
		cmd = { 'TodoTrouble', 'TodoTelescope' },
		keys = {
			{ ']t', '<cmd>lua require(\'todo-comments\').jump_next()<cr>' },
			{ '[t', '<cmd>lua require(\'todo-comments\').jump_prev()<cr>' },
		}
	},
	{
		'ahmedkhalf/project.nvim',
		main = 'project_nvim',
		opts = {
			patterns = { '>Sources' },
		},
	},
	{
		'lambdalisue/suda.vim',
		lazy = false,
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

			cmp.setup {
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
			}

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
			'RRethy/nvim-treesitter-endwise',
			'andrewferrier/textobj-diagnostic.nvim',
			'szebniok/tree-sitter-wgsl',
			'HiPhish/nvim-ts-rainbow2',
			'JoosepAlviste/nvim-ts-context-commentstring',
		},
		build = ':TSUpdate',
		config = function()
			require('nvim-treesitter.configs').setup {
				ensure_installed = TREESITTER_PARSERS,
				highlight = {
					enable = true,
				},
				autotag = {
					enable = true,
				},
				endwise = {
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
			'folke/noice.nvim',
			'rcarriga/nvim-notify',
			'AckslD/nvim-neoclip.lua',
		},
		keys = {
			{ '<leader>b',  '<cmd>Telescope buffers<cr>' },
			{ '<leader>n',  '<cmd>Telescope notify<cr>' },
			{ '<leader>t',  '<cmd>Trouble<cr>' },
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

			telescope.load_extension('noice')
			telescope.load_extension('notify')
			telescope.load_extension('projects')
			telescope.load_extension('neoclip')
		end
	},
	{
		'folke/noice.nvim',
		lazy = false,
		dependencies = {
			'smjonas/inc-rename.nvim',
			'hrsh7th/nvim-cmp',
			'MunifTanjim/nui.nvim',
			'rcarriga/nvim-notify',
		},
		opts = {
			lsp = {
				override = {
					['vim.lsp.util.convert_input_to_markdown_lines'] = true,
					['vim.lsp.util.stylize_markdown'] = true,
					['cmp.entry.get_documentation'] = true,
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
			local function on_attach(client, bufnr)
				require('lsp-format').on_attach(client)

				local bufopts = { noremap = true, silent = true, buffer = bufnr }

				vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, bufopts)
				vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, bufopts)
				vim.keymap.set('n', ']d', vim.diagnostic.goto_next, bufopts)
				vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, bufopts)

				vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
				vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
				vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
				vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
				vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
				vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bufopts)
				vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
				vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
				vim.keymap.set('n', '<leader>rn', ':IncRename ', bufopts)
				vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
				vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
				vim.keymap.set('n', '<leader><C-f>', function()
					vim.lsp.buf.format { async = true }
				end, bufopts)
			end

			local capabilities = require('cmp_nvim_lsp').default_capabilities()
			local s = { on_attach = on_attach, capabilities = capabilities }

			local lspc = require('lspconfig')

			for _, v in pairs(LSP_SERVERS) do
				lspc[v].setup(s)
			end
		end
	},
	{
		'rcarriga/nvim-notify',
		config = function()
			vim.notify = require('notify')

			vim.notify.setup {
				fps = 60,
				stages = 'fade',
				render = 'compact',
				timeout = 2000,
			}
		end
	},
	{
		'windwp/nvim-autopairs',
		opts = {
			check_ts = true,
			enable_check_bracket_line = false,
			disable_in_replace_mode = false,
		},
	},
	{
		'jose-elias-alvarez/null-ls.nvim',
		config = function()
			local null_ls = require('null-ls')

			for idx, key in ipairs(NULL_LSP_PROGRAMS) do
				NULL_LSP_PROGRAMS[idx] = null_ls.builtins.formatting[key]
			end

			null_ls.setup {
				sources = NULL_LSP_PROGRAMS,
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
	'JafarDakhan/vim-gml',
	'jghauser/mkdir.nvim',
}, {
	dev = {
		path = '~/Sources',
	},
	checker = {
		enabled = true,
		frequency = 3600,
	},
})
