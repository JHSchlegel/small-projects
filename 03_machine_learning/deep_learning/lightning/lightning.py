import torch
import torch.nn as nn
import torch.nn.functional as F
import torchvision
import torchvision.transforms as transforms
import matplotlib.pyplot as plt 
import pytorch_lightning as pl
from pytorch_lightning import Trainer, seed_everything
from torchinfo import summary
import torchmetrics
import numpy as np

if torch.cuda.is_available():
    print("Things will be fast")

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

train_transforms =  transforms.Compose([transforms.Resize(256),
     # if size is int, smaller edge of image will be matched to this int
     transforms.RandomCrop((224, 224)),
     transforms.ToTensor()])

val_transforms =  transforms.Compose([transforms.Resize(256),
     # if size is int, smaller edge of image will be matched to this int
     transforms.CenterCrop((224, 224)),
     transforms.ToTensor()])

test_transforms =  transforms.Compose([transforms.Resize(224),
     # if size is int, smaller edge of image will be matched to this int
     transforms.ToTensor()])

train_set = torchvision.datasets.MNIST(root = './Data', download=True, train = True,
                                       transform = train_transforms)
test_set =  torchvision.datasets.MNIST(root = './Data', download=True, train = False,
                                       transform = test_transforms)

train_subset, val_subset = torch.utils.data.random_split(
        train_set, [50000, 10000], generator=torch.Generator().manual_seed(1))

train_loader = torch.utils.data.DataLoader(dataset = train_subset, batch_size = 64, shuffle = True,
                                           num_workers = 32)
val_loader = torch.utils.data.DataLoader(dataset = val_subset, batch_size = 64, shuffle = False,
                                           num_workers = 32)
test_loader = torch.utils.data.DataLoader(dataset = test_set, batch_size = 128, shuffle = False,
                                          num_workers = 32)



trainiter = iter(train_loader)
images, labels = next(trainiter)
print(images.size())
fig = plt.figure(figsize = (15, 5))
for idx in np.arange(20):
    ax = fig.add_subplot(4, int(20/4), idx + 1, xticks = [], yticks = [])
    ax.imshow(np.squeeze(images[idx]))
    ax.set_title(labels[idx].item())
    fig.tight_layout()
plt.savefig("MNIST.png")
plt.show()


class AlexNet(pl.LightningModule):
    def __init__(self, num_classes = 10):
        super(AlexNet, self).__init__()
        self.conv = nn.Sequential(
            nn.LazyConv2d(96, kernel_size=11, stride=4, padding=1),
            nn.ReLU(), nn.MaxPool2d(kernel_size=3, stride=2),
            nn.LazyConv2d(256, kernel_size=5, padding=2), nn.ReLU(),
            nn.MaxPool2d(kernel_size=3, stride=2),
            nn.LazyConv2d(384, kernel_size=3, padding=1), nn.ReLU(),
            nn.LazyConv2d(384, kernel_size=3, padding=1), nn.ReLU(),
            nn.LazyConv2d(256, kernel_size=3, padding=1), nn.ReLU(),
            nn.MaxPool2d(kernel_size=3, stride=2))
        self.fc = nn.Sequential(nn.Flatten(),
            nn.LazyLinear(4096), nn.ReLU(), nn.Dropout(p=0.5),
            nn.LazyLinear(4096), nn.ReLU(),nn.Dropout(p=0.5),
            nn.LazyLinear(num_classes)) 
        self.train_acc = torchmetrics.classification.Accuracy(task="multiclass", num_classes=num_classes)
        self.valid_acc = torchmetrics.classification.Accuracy(task="multiclass", num_classes=num_classes)
        self.test_acc = torchmetrics.classification.Accuracy(task="multiclass", num_classes=num_classes)
    def forward(self, x):
        return self.fc(self.conv(x))    
    def training_step(self, batch, batch_idx):
        # training_step defines the train loop.
        # it is independent of forward
        x, t = batch
        z = self.forward(x)
        loss = F.cross_entropy(z, t)
        acc = self.train_acc(z, t)
        self.log('train_loss', loss, on_epoch = True)
        self.log('train_acc', acc, on_epoch = True)
        return loss
    def validation_step(self, batch, batch_idx):
        # it is independent of forward
        x, t = batch
        z = self.forward(x)
        loss = F.cross_entropy(z, t)
        acc = self.valid_acc(z, t)
        self.log("val_loss", loss, on_epoch=True)
        self.log("val_acc", acc, on_epoch=True)
        #return {'val_loss': loss}
    def test_step(self, batch, batch_idx):
        # it is independent of forward
        x, t = batch
        z = self.forward(x)
        loss = F.cross_entropy(z, t)
        acc = self.test_acc(z, t)
        self.log("test_loss", loss, on_epoch=True)
        self.log("test_acc", acc, on_epoch=True)

    def configure_optimizers(self):
        return torch.optim.Adam(self.parameters(), lr=1e-3)
    
if __name__ == "__main__":
    torch.set_float32_matmul_precision('medium')
    seed_everything(42, workers=True)
    num_epochs = 10
    model = AlexNet(10)
    print(summary(model, (1,1,224,224)))
    trainer = Trainer(accelerator = "gpu", max_epochs= num_epochs, fast_dev_run = False, log_every_n_steps= 2,
                      deterministic = True) # fast_dev_run to test whether model works correctly or not
    trainer.fit(model=model, train_dataloaders=train_loader, val_dataloaders=val_loader)
    trainer.test(dataloaders=test_loader)
    
    