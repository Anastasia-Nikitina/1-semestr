using System;
using System.IO;
using System.Threading.Tasks;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Threading;
using Avalonia.Markup.Xaml;
using BigIntCalculator;

using static BigIntCalculator.Interpreter;

namespace GUI.Views
{
    public class MainWindow : Window
    {
        private readonly TextBox _inputCode;
        private readonly TextBox _console;
        private readonly MenuItem _run;
        private string _filePath;
        private readonly Grid _grid;
        private bool _isPressedCtrl;
        
        public MainWindow()
        {
            InitializeComponent();
            printed. Subscribe(PrintToConsole);
            _inputCode = this.Find<TextBox>("InputCode");
            _console = this.Find<TextBox>("Console");
            _run = this.FindControl<MenuItem>("Run");
            _grid = this.FindControl<Grid>("Grid");
            _filePath = "";
            _grid.KeyDown += SaveKeyBoardEvent;
            _grid.KeyUp += LetterPress;
        }
        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
        
        private void PrintToConsole(string msg)
        {
            Dispatcher.UIThread.Post(() => 
                _console.Text += msg + "\n");
        }
        private void SaveKeyBoardEvent(object? sender, KeyEventArgs e)
        {
            if (e.Key != Key.LeftCtrl) return;
            _isPressedCtrl = true;
        }
        
        void LetterPress(object? sender, KeyEventArgs e)
        {
            if (_isPressedCtrl)
            {
                switch (e.Key)
            {
                case Key.S:
                    Save();
                    break;
                case Key.O:
                    Open();
                    break;
                case Key.N:
                    New();
                    break;
            }
            _isPressedCtrl = false;
            }
        }
        
        public async void Open()
        {
            Save();
            var dialog = new OpenFileDialog();
            dialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var path = await dialog.ShowAsync(this);
            if (path is not {Length: > 0}) return;
            _inputCode.Text = await File.ReadAllTextAsync(path[0]);
            _filePath = path[0];
        }
        
        private async void Save()
        {
            if (!string.IsNullOrEmpty(_filePath))
            {
                await File.WriteAllTextAsync(_filePath, _inputCode.Text);
            }
            var dialog = new SaveFileDialog
            {
                InitialFileName = _filePath
            };
            var path = await dialog.ShowAsync(this);
            if (path != null)
            {
                await File.WriteAllTextAsync(path, _inputCode.Text);
                _filePath = path;
            }
        }
        
        public void New()
        {
            Save();
            _inputCode.Text = "";
            _filePath = "";
            Save();  
            
        }
        
        private void Run(object sender, RoutedEventArgs e)
        {
            if (string.IsNullOrEmpty(_inputCode.Text))
            {
                _inputCode.Text = "Empty";
                return;
            }
            _run.IsEnabled = false;
            var task = new Task(() =>
            {
                try
                {
                    runPrint(parse(_inputCode.Text));
                    Dispatcher.UIThread.Post(() =>
                    {
                        _run.IsEnabled = true;
                        _console.Text += "Process finished" + "\n";
                    });
                }
                catch (Exception except)
                {
                    Dispatcher.UIThread.Post(() =>
                    {
                        _console.Text += "Process failed:" + "\n";
                        _console.Text += except.Message + "\n";
                        _run.IsEnabled = true;
                    });
                }
            });
            task.Start();
        }
        
        public void GoNew(object sender, RoutedEventArgs e)
        {
            New();
        }
        
        public void GoOpen(object sender, RoutedEventArgs e)
        {
            Open();
        }

        private void GoSave(object sender, RoutedEventArgs e)
        {
            Save();
        }

    }
}
